{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Calamity
import Calamity.Cache.InMemory
import qualified Calamity.Types.Model.Channel.Message as Cont
import qualified Calamity.Types.Tellable (ToMessage)
import Calamity.Commands
import Calamity.Commands.Context (useFullContext, ctxMessage, FullContext, CalamityCommandContext (ctxChannelID, ctxUserID))
import qualified Calamity.HTTP.Channel as HTTPC
import qualified Calamity.Interactions as I
import qualified Calamity.HTTP.Internal.Request as HTTPI
import Calamity.Metrics.Noop
import Control.Concurrent
import Optics
import Control.Monad
import qualified Data.Text as T
import qualified Di
import qualified DiPolysemy as DiP
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.State as P
import qualified Polysemy.Internal.Union as PU
import System.Environment (getEnv)
import TextShow
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust)
import GHC.Data.ShortText (ShortText(contents))
import qualified Calamity as HTTPC
import Web.Authenticate.OAuth (def)
import Data.Monoid (Endo)
import Data.Function
import qualified Data.Text.Lazy as DT
import Calamity.Cache.Eff (updateMessage, getMessage)
import Control.Monad.State (evalState, MonadIO (liftIO), MonadState)
import Control.Concurrent.Thread.Delay (delay)
import Data.List (sort, sortOn)
import qualified Data.Default as D (def, Default)
import Polysemy.Error (Error)

-- Custom view states and the values they contain
data CustomViewState = CustomViewState
  { numOptions :: Int
  , selected_ :: Maybe T.Text
  }

data PollViewState = PollViewState
  { options_ :: [T.Text]
  , selected :: Maybe T.Text
  , voted :: [T.Text]
  }

$(makeFieldLabelsNoPrefix ''CustomViewState)
$(makeFieldLabelsNoPrefix ''PollViewState)

-- Constants
annoying :: Bool
annoying = True

-- Emojies used for polls
defaultRawEmoji :: [RawEmoji]
defaultRawEmoji = map (UnicodeEmoji . T.pack) 
    [ "0️⃣"
    , "1️⃣"
    , "2️⃣"
    , "3️⃣"
    , "4️⃣"
    , "5️⃣"
    , "6️⃣"
    , "7️⃣"
    , "8️⃣"
    , "9️⃣"
    ]

-- Simply finds all possible pairs in the range 1..x
-- On nothing, return impossible permutation
perm :: Maybe Int -> [(Int, Int)]
perm Nothing = [(-1,-1)]
perm (Just x) = [(g, h) | g <- [1..x], h <- [1..x]]

-- Reads the string as a number.
-- Gives nothing if no number is found.
permHelp :: String -> Maybe Int
permHelp x
  | null f = Nothing
  | otherwise = Just (read f)
    where f = takeWhile isDigit x

-- Returns all primes from 1 to limit that are not prime
antiPrimes :: Int -> [Int]
antiPrimes limit = [x | x <- [3..limit], not (checkPrime x)]

-- Checks if number is prime
checkPrime :: Int -> Bool
checkPrime x = x >= 1 && null [1 | y <- [2..x `div` 2 + 1], x `mod` y == 0]

-- Splits message after limit characters at the latest.
-- If the remainder is longer than limit, splits that as well.
splitMessage :: Int -> String -> [String]
splitMessage _ [] = []  
splitMessage limit orig 
  | length orig < limit = [orig]
  | otherwise = [take (limit-1) orig] <> splitMessage limit  (drop (limit-1) orig)

-- Splits message at the latest character that matches character 
-- that is before mx
splitAtAfter :: Int -> Char -> String -> [String]
splitAtAfter _ _ [] = []
splitAtAfter mx char str = case snd x of 
                "" -> [fst x]
                st -> [(reverse . dropWhile (/=char) . reverse . fst) x] <> splitAtAfter mx char ((reverse . takeWhile (/=char) . reverse . fst) x <> snd x)
              where x = splitAt mx str 

-- Splits string at all occurrances of any of the characters in c
splitAtAll :: String -> String -> [String]
splitAtAll _ [] = []
splitAtAll [] (s:st) = [s] : splitAtAll [] st 
splitAtAll c str@(s:st) =
  case dropWhile (`elem` c) str of
    "" -> []
    s' -> s'' : splitAtAll c rem
      where (s'', rem) = break (`elem` c) s'


-- Count votes in the poll command that uses views.
countVotes :: [String] -> [(String, Int)] -> [(String, Int)]
countVotes [] yss = yss
countVotes (x:xs) yss@(y:ys)
  | x `elem` map fst yss = countVotes xs ([(l, lx + 1) | (l, lx) <- yss, l == x] ++ [(l, lx) | (l, lx) <- yss, l /= x])
  | otherwise = countVotes xs ((x,1):yss)

-- Reload the message in cache and return a list of reactions done to the message 
getReactions :: (BotC r) => Message -> P.Sem r [Reaction]
getReactions msg = do
  msg' <- getMessage (view #id msg)
  case msg' of 
    Just m -> pure (view #reactions m)
    Nothing -> pure []

-- Combining T.Text and RawEmoji to sendable text
combineTextEmoji :: [T.Text] -> [RawEmoji] -> T.Text
combineTextEmoji _ [] = ""
combineTextEmoji [] _ = ""
combineTextEmoji (t:text) (e:emojis) = t <> T.pack " " <> showt e <> T.pack "\n" <> combineTextEmoji text emojis

-- Count up votes for each of the different categories
-- Used in the poll command
tally :: [RawEmoji] -> [T.Text] -> [Reaction] -> [(Integer, RawEmoji, T.Text)]
tally emojies categories reactions = [(view #count react, emoji', category) | (emoji', category) <- contestants, react <- reactions, view #emoji react == emoji', view #me react] 
                                      where
                                        contestants = zip emojies categories

-- "Understands" the result from tally, and generates a winning message that 
-- can be sent to announce the winners of the poll
handlePollResults :: [RawEmoji] -> [T.Text] -> [Reaction] -> T.Text
handlePollResults emojies categories reactions = 
      let sortedTally = sortOn (\(x, y, z) -> x*(-1)) $ tally emojies categories reactions
      in let mx = (\(x, _, _) -> x) $ head sortedTally
      in let winners = takeWhile (\(x, _, _) -> x == mx) sortedTally
      in case length winners of
              1 -> "Congratulations to `" <> (categoryGetter . head) winners <> "` for winning this poll with " <> (showt . votesGetter . head) winners <> " votes!"
              n -> "Congratulations to " <> T.unwords (map ((\x -> "`" <> x <> "`,") . categoryGetter) winners) <> " for a " <> showt n <> " way tie, with " <> (showt . votesGetter . head) winners <> " votes!"
              where 
                categoryGetter (_, _, x) = x
                votesGetter    (x, _, _ ) = x


-- Pad categories to be of equal length (if only Discord used a monospaced font)
padCategories :: [T.Text] -> [T.Text]
padCategories words = map T.pack $ padCategoriesHelper (map T.unpack words) 0

-- The actual brain of padCategories.
-- Ensures that the strings are of equal length.
-- The '-' symbok is used because Discord embeds remove excessive whitespace
padCategoriesHelper :: [String] -> Int -> [String]
padCategoriesHelper [] len = []
padCategoriesHelper words len
  | all ((==len) . length) words = words
  | any ((> len) . length) words = padCategoriesHelper words (foldl max 0 $ map length words)
  | otherwise = [word ++ ['-' | _ <- [0..((+1) $ len - length word)]] | word <- words]

main :: IO ()
main = do
  -- Retrieves the authentication token from the environment variable "BOT_TOKEN"
  -- If running it yourself and have a variable with a different name, simply replace the below with said name
  token <- T.pack <$> getEnv "BOT_TOKEN"
  

  -- The main loop
  Di.new $ \di -> -- Enables logging
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di 
      . runCacheInMemory   
      . runMetricsNoop
      . useConstantPrefix "$" -- Prefix to be used before commands
      . useFullContext        -- How much info for each event is available to the bot. 
      $ runBotIO (BotToken token) defaultIntents $ do
        addCommands $ do      -- Allows adding commands

          command @'[] "apple" \ctx -> do
            void . tell @T.Text ctx $ "Apple"
          
          -- Returns all permutations of pairs from 1..x
          command @'[T.Text] "perm" \ctx ans -> do
            let stringed = show $ perm $ permHelp $ T.unpack ans
            -- Splits the string so that each message fits within 
            -- the 2000 character limit of Discord
            let splitted = splitAtAfter 2_000 ')' stringed
            -- After all the messages are created, send them all
            mapM_ (tell @T.Text ctx . T.pack) splitted 

          -- All numbers up to ans that is not prime are returned
          command @'[T.Text] "antiprime" \ctx ans -> do
            let x = takeWhile isDigit (T.unpack ans)
            -- Ensures all messages fit within 2000 character limit
            -- and ensures no numbers are broken up over multiple messages
            let c = splitAtAfter 2_000 ',' $ show $ antiPrimes $ fromMaybe 1 (if null x then Nothing else Just (read x))
            let m = map (void . reply @T.Text ctx . T.pack) c 
            sequence_ m
          
          -- A command that is used solely for testing and understanding the library
          -- Can be largely ignored
          command @'[T.Text] "poll-testing" \ctx poll -> do
            -- This works, as 'poll' can only contain a singular text-value?
            let categories = splitAtAll "-_." $ T.unpack poll
            void $ DiP.info $ T.pack "apple"
            void $ DiP.info poll
            void $ DiP.info $ T.pack "Now showing categories:"
            void $ DiP.info $ T.pack $ show categories
            void $ DiP.info $ Cont.content $ ctxMessage ctx 
            -- This works to get all the categories
            let newCategories = tail . words . show . Cont.content $ ctxMessage ctx
            mapM_ (DiP.info . T.pack) newCategories
            let doThis = map (void . DiP.info . T.pack) categories
            sequence_ doThis
            -- What is included in the ctx, or fullContext, in this case?
            void $ DiP.info $ show ctx
            -- Testing different way to create a message
            let emptyOption = SelectOption "" "" Nothing Nothing False
            let options' = [emptyOption & #label .~ T.pack "Test1" & #default_ .~ True, emptyOption & #label .~ T.pack "Test2"] :: [SelectOption]
            let newMessage = intoMsg (DT.pack "Apple") -- <> intoMsg ((Select options' Nothing Nothing Nothing False (CustomID "Bot 13")) :: Select) 
            void . DiP.info $ T.pack "Trying to send message"
            void . DiP.info $ T.pack $ show $ runToMessage newMessage
            void $ tell ctx $ runToMessage newMessage 
            let msg = ctxMessage ctx
            
            void $ DiP.info $ T.pack $ show msg
            void $ DiP.info @T.Text "Now printing the thing:"
            -- How I originally believed emojies worked
            void $ updateMessage (getID $ ctxMessage ctx) (\x -> x {reactions = [Reaction 1 True $ UnicodeEmoji $ T.pack ":one:"]})
            let reacts = HTTPC.GetReactions (ctxChannelID ctx) (ctxMessage ctx)
            void $ tell @T.Text ctx "text"
 
          -- A simple view that allows selecting between 3 options.
          -- Does not provide functionality beyond that
          command @'[] "test-view" \ctx-> do 
            let emptyOption = SelectOption "" "" Nothing Nothing False
            let options' = [emptyOption & #label .~ T.pack "Test1" & #default_ .~ True, emptyOption & #label .~ T.pack "Test2"] :: [SelectOption]
            let view = I.row do
                  a <- I.select $ map T.pack ["Apple", "Jeans", "Boots"] 
                  pure a
              
            I.runView view (tell ctx) $ \a -> do
              case a of
                Just "Apple" -> void $ I.respond $ T.pack "No" 
                _ -> void $ I.respondEphemeral $ T.pack "Pie"
              void $ DiP.info $ show a
              void $ I.endView view
            pure ()

          -- Only used on a server with several other bots running
          command @'[T.Text] "view" \ctx stats-> do
            when (stats == "stats") $ void $ tell @T.Text ctx "$exp" 

          -- The poll function. Integer is time to sleep, T.Text is a list of
          -- space-separated values that are to be voted over. 
          -- Currently supports up to 10 elements in said list
          command @'[Integer, T.Text] "poll" \ctx sleep arg -> do
            -- Gets the entire message as it was written
            let content = T.unpack $ view #content $ ctxMessage ctx
            -- When splitting at spaces, the 2 first elements are the command itself, and the sleep amount
            -- The remainder will be the categories to be voted over
            let categories = map T.pack $ drop 2 $ splitAtAll " " content
            let emojies = take (length categories) defaultRawEmoji
            -- Construct message
            -- Create an embedded message that contains the alternatives of the poll
            let embedded = (def :: Embed) & #title ?~ "Poll" & #description ?~ combineTextEmoji (padCategories categories) emojies

            -- Send the poll and get a reference to it
            sentMessage <- tell ctx $ intoMsg embedded <> intoMsg @T.Text "Polled"

            case sentMessage of 
              -- Only when the message is sent
              Right s -> do
                          void . DiP.info . T.pack $ "Trying to rect to messageID " ++ show (view #id s) ++ " in channelID " ++ show (view #channelID s)
                          -- React to the sent message with the poll alternatives
                          let requests = map (HTTPC.CreateReaction s s) emojies 
                          mapM_ invoke requests -- React to own message to create "vote" buttons

                          -- Once message has been reacted to, wait for poll to finish
                          liftIO $ delay (sleep * 1_000_000)
                          reactions <- getReactions s :: (BotC r) => P.Sem r [Reaction]
                          -- Tally votes
                          void . tell @T.Text ctx $ handlePollResults emojies categories reactions
                          
          -- If message contains "haskell", react to said message 
          react @'MessageCreateEvt \(msg, _usr, _member) -> do
            when (T.isInfixOf "haskell" $ view #content msg) do
              void . invoke $ CreateReaction msg msg (UnicodeEmoji "1️⃣")
          
          -- Annoying function.
          -- When someone starts typing, ask if they are gonna finish their message.
          -- The constant "annoying" can be disabled if testing your own bot
          react @'TypingStartEvt \(channelFlake, userFlake, _) -> do
            when annoying $
              void . tell @T.Text channelFlake $ T.pack . ("Are you gonna finish that "++) . (++" ?") $ T.unpack . mention $ userFlake
          
          -- A different poll function using a drop-down instead.
          -- Greatly increases the amount of items that can be polled, and does so effortlessly.
          -- However, everyone can vote for the same thing several times
          command @'[] "poll2" \ctx -> do
            -- Get original message
            let messageContent = T.unpack $ ctxMessage ctx ^. #content
            -- The things to be polled are the last elements, as the first
            -- will always be the command
            let components = tail $ words messageContent 
            -- The time feature does not currently work
            let time = takeWhile isDigit $ head components 
            if null time then do 
                void $ tell @T.Text ctx  "Format: <time> <category1> <category2> ..."  
            else do 
              let timeInt = read time :: Int
              let categories = map T.pack $ tail components
              -- Create buttons to add to counter and finish poll respectively
              let view options = do
                      ~(vote, done) <- I.row do
                        vote <- I.button ButtonPrimary "Vote"
                        done <- I.button ButtonPrimary "Done"
                        pure (vote, done)
                      s <- I.select options
                      pure (vote, done, s)
              -- Using custom poll-view-state that contains the desired information
              let initialState = PollViewState categories (Just (head categories)) []
              s <- P.evalState initialState $ 
                I.runView (view categories) (tell ctx) \(vote, done, s) -> do
                  -- When someone calls vote on something
                  when vote do
                      -- Get what they currently have selected
                      sel <- P.gets (^. #selected)
                      votes <- P.gets (^. #voted)
                      -- Modify vodets to reflect this
                      P.modify' (#voted .~ (votes ++ [fromMaybe (head categories) sel]))
                      -- Update view
                      I.replaceView (view categories) (void . I.edit)
                  -- When someone selects done
                  when done do
                      finalSelected <- P.gets (^. #selected)
                      votes <- P.gets (^. #voted)
                      -- View has to end on something
                      I.endView finalSelected
                      I.deleteInitialMsg
                      -- Logic to view votes and result nicely here
                      let countedVotes = countVotes (map T.unpack votes) []
                      void . I.respond $ show countedVotes 
                      -- Send which votes were cast as a list
                      void . tell @T.Text ctx $ T.pack $ show votes 

                  -- Handle selection
                  case s of
                    -- New element selected
                    Just s' -> do
                      P.modify' (#selected ?~ s')
                      void I.deferComponent
                    -- Nothign selected
                    Nothing -> pure ()
              P.embed $ print s
              pure ()

