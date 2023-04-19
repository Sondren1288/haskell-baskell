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
import Control.Monad.State (evalState, MonadIO (liftIO))
import Control.Concurrent.Thread.Delay (delay)
-- import Control.Lens

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
defaultRawEmoji :: [RawEmoji]
-- defaultRawEmoji = [(UnicodeEmoji . T.pack) (":" ++ show x ++ ":") | x <- [0..9]]
defaultRawEmoji = map (UnicodeEmoji . T.pack) 
    [ ":zero:"
    , ":one:"
    , ":two:"
    , ":three:"
    , ":four:"
    , ":five:"
    , ":six:"
    , ":seven:"
    , ":eight:"
    , ":nine:"] 

perm :: Maybe Int -> [(Int, Int)]
perm Nothing = [(-1,-1)]
perm (Just x) = [(g, h) | g <- [1..x], h <- [1..x]]

permHelp :: String -> Maybe Int
permHelp x
  | null f = Nothing
  | otherwise = Just (read f)
    where f = takeWhile isDigit x

antiPrimes :: Int -> [Int]
antiPrimes limit = [x | x <- [3..limit], not (checkPrime x)]


checkPrime :: Int -> Bool
checkPrime x = x >= 1 && null [1 | y <- [2..x `div` 2 + 1], x `mod` y == 0]

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

splitAtAll :: String -> String -> [String]
splitAtAll _ [] = []
splitAtAll [] (s:st) = [s] : splitAtAll [] st 
splitAtAll c str@(s:st) =
  case dropWhile (`elem` c) str of
    "" -> []
    s' -> s'' : splitAtAll c rem
      where (s'', rem) = break (`elem` c) s'

data Calc a
  = Num a
  | Plu (Calc a) (Calc a) 
  | Min (Calc a) (Calc a) 
  | Div (Calc a) (Calc a) 
  | Mul (Calc a) (Calc a) 
  | Void
  deriving (Show, Eq)

tokenize :: (Num a) => String -> String -> String -> Calc a
tokenize fstArg token sndArg
  | token == "+" = undefined
  | token == "-" = undefined
  | token == "/" = undefined
  | token == "*" = undefined
  | otherwise = undefined 

parseString :: (Num a) => (Calc a, String) -> (Calc a, String)
parseString (a, []) = (a, [])
parseString (calc, str@(s:st))
  | s == '+' = undefined

parseStringCalculator ::(Num a) => [String] -> Calc a
parseStringCalculator = undefined
--parseStringCalculator (stf : "(" : ste) = parseStringCalculator ste
--parseStringCalculator (stf : ")": ste) = parseStringCalculator stf
--parseStringCalculator str@(s:st)  
--  | s == "(" = undefined -- Find corresponidng ')'
--  | s == ")" = undefined -- Found corresponding
--  | s == "+" = undefined 
--  | s == "-" = undefined
--  | s == "/" = undefined
--  | s == "*" = undefined
--  | s == " " = undefined
--  | s == "," = undefined
--  | s == "." = undefined
--  | otherwise = takeWhile isDigit str

errorNotMessage :: Either RestError Message -> Bool
errorNotMessage (Left  _) = True
errorNotMessage (Right _) = False


countVotes :: [String] -> [(String, Int)] -> [(String, Int)]
countVotes [] yss = yss
countVotes (x:xs) yss@(y:ys)
  | x `elem` map fst yss = countVotes xs ([(l, lx + 1) | (l, lx) <- yss, l == x] ++ [(l, lx) | (l, lx) <- yss, l /= x])
  | otherwise = countVotes xs ((x,1):yss)
--sendMsgTester :: (BotC r, ToMessage msg, Tellable t) => t -> msg -> P.Sem r (Either RestError Message)
--sendMsgTester context msg = do
--  let msg_ = (tell context msg :: (BotC r) => P.Sem r (Either RestError Message))
--  let msg = P.run msg_
--  --msg <- P.get
--  if errorNotMessage $ msg then
--    return msg
--  else
--    return msg
----  new <- P.get :: Either RestError Message
----  if errorNotMessage new then
----    return new
----  else
----    return new

eitherToMessage :: Either RestError Message -> Maybe Message
eitherToMessage (Left x) = Nothing
eitherToMessage (Right msg) = Just msg
    
getReactions :: (BotC r) => Message -> P.Sem r [Reaction]
getReactions msg = do
  msg' <- getMessage (view #id msg)
  case msg' of 
    Just m -> pure (view #reactions m)
    Nothing -> pure []

combineTextEmoji :: [T.Text] -> [RawEmoji] -> T.Text
combineTextEmoji _ [] = ""
combineTextEmoji [] _ = ""
combineTextEmoji (t:text) (e:emojis) = t <> T.pack " " <> showt e <> T.pack "\n" <> combineTextEmoji text emojis


main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useConstantPrefix "$"
      . useFullContext
      $ runBotIO (BotToken token) defaultIntents $ do
        addCommands $ do
          -- just some examples

          command @'[] "apple" \ctx -> do
            void . tell @T.Text ctx $ "Apple"
          
          command @'[T.Text] "perm" \ctx ans -> do
            let stringed = show $ perm $ permHelp $ T.unpack ans
            let splitted = splitAtAfter 2000 ')' stringed
            mapM_ (tell @T.Text ctx . T.pack) splitted 

          command @'[T.Text] "antiprime" \ctx ans -> do
            let x = takeWhile isDigit (T.unpack ans)
            let c = splitAtAfter 2000 ',' $ show $ antiPrimes $ fromMaybe 1 (if null x then Nothing else Just (read x))
            let m = map (void . reply @T.Text ctx . T.pack) c 
            sequence_ m
          
          command @'[T.Text] "calculate" \ctx ans -> do
            undefined

          command @'[T.Text] "poll" \ctx poll -> do
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
            -- Create new message            
            -- Read reactions after threadDelay
            -- send results
            --let cont = CreateMessageOptions {HTTPC.content = T.pack "Apple"}
            --let x = HTTPC.CreateMessage cont 
            --let x = "apple"
            -- let x = reply @T.Text ctx $ T.pack "apple"
            void $ DiP.info $ show ctx
            let emptyOption = SelectOption "" "" Nothing Nothing False
            let options' = [emptyOption & #label .~ T.pack "Test1" & #default_ .~ True, emptyOption & #label .~ T.pack "Test2"] :: [SelectOption]
            let newMessage = intoMsg (DT.pack "Apple") -- <> intoMsg ((Select options' Nothing Nothing Nothing False (CustomID "Bot 13")) :: Select) 
            -- intoMsg ((def & #description ?~ "Banan") :: Embed ) 
            void . DiP.info $ T.pack "Trying to send message"
            void . DiP.info $ T.pack $ show $ runToMessage newMessage
            void $ tell ctx $ runToMessage newMessage -- :: (BotC r, z) => PU.Member r z 
            let msg = ctxMessage ctx
            
            void $ DiP.info $ T.pack $ show msg
            --reactions $ ctxMessage ctx
            -- let b = HTTPC.CreateReaction (ctxChannelID ctx) (ctxMessage ctx) (UnicodeEmoji (T.pack ":one:") :: RawEmoji)
            -- void $ HTTPI.invoke b
            void $ DiP.info @T.Text "Now printing the thing:"
            void $ updateMessage (getID $ ctxMessage ctx) (\x -> x {reactions = [Reaction 1 True $ UnicodeEmoji $ T.pack ":one:"]})
            let reacts = HTTPC.GetReactions (ctxChannelID ctx) (ctxMessage ctx)
            void $ tell @T.Text ctx "text"
            -- a <- P.get
            -- tester <- P.gets (^. #context)
            -- Cont.Message
            -- pure ()

          command @'[] "tast" \ctx-> do 
            let emptyOption = SelectOption "" "" Nothing Nothing False
            let options' = [emptyOption & #label .~ T.pack "Test1" & #default_ .~ True, emptyOption & #label .~ T.pack "Test2"] :: [SelectOption]
            let view = I.row do
                  a <- I.select $ map T.pack ["Apple", "Jeans", "Boots"] 
                  pure a
              
            I.runView view (tell ctx) $ \(a) -> do
              case a of
                Just "Apple" -> void $ I.respond $ T.pack "No" 
                _ -> void $ I.respondEphemeral $ T.pack "Pie"
              void $ DiP.info $ show a
              void $ I.endView view
              
            
            pure ()

          command @'[T.Text] "view" \ctx stats-> do
            when (stats == "stats") $ void $ tell @T.Text ctx "$exp" 

          command @'[T.Text] "poll2" \ctx poll -> do
            void $ DiP.info poll

          command @'[Integer, T.Text] "experiment" \ctx sleep arg -> do
            let content = T.unpack $ view #content $ ctxMessage ctx
            let categories = map T.pack $ drop 2 $ splitAtAll " " content
            let emojies = take (length categories) defaultRawEmoji
            -- Construct message
            -- let messageToSend = intoMsg "Poll" <> intoMsg Embed (def & #description ?~ "Embed description")
            let embedded = (def :: Embed) & #title ?~ "Poll" & #description ?~ combineTextEmoji categories emojies


            sentMessage <- tell ctx $ intoMsg embedded <> intoMsg @T.Text "Normal text"
            -- sentMessage <- tell @T.Text ctx $ T.pack $ show categories ++ "\nEnds after " ++ show sleep ++ " seconds"
            case sentMessage of 
              Right s -> do
                          void $ updateMessage (view #id s) (\x -> x {reactions = map (Reaction 1 True) emojies})
                          let tempSnowflake = Snowflake 1097858537608196138 :: Snowflake Message 
                          let msgSnowflake = view #id s
                          let channelSnowflake = view #channelID s :: Snowflake Channel
                          -- reactions <- getMessage channelSnowflake msgSnowflake
                          -- let reactions = view #reactions s
                          liftIO $ delay (sleep * 1_000_000)
                          reactions <- getReactions s :: (BotC r) => P.Sem r [Reaction]
                          --reactions <- getMessage (fromMaybe tempSnowflake $ Cont.messageID s)
                          void $ tell @T.Text ctx $ T.pack $ "Reactions " <> show (length reactions) <> " "
                
            --when Right sentMessage do
            --  reactions <- getMessage (#messageID ^. Right sentMessage) --(fromJust $ messageGuildID sentMessage)
            --  void $ tell @T.Text ctx $ T.pack $ show reactions

          react @'TypingStartEvt \(channelFlake, userFlake, _) -> do
            void . tell @T.Text channelFlake $ T.pack . ("Are you gonna finish that "++) . (++" ?") $ T.unpack . mention $ userFlake
          
          command @'[] "poll3" \ctx -> do
            let messageContent = T.unpack $ ctxMessage ctx ^. #content
            let components = tail $ words messageContent 
            let time = takeWhile isDigit $ head components 
            if null time then do 
                void $ tell @T.Text ctx  "Format: <time> <category1> <category2> ..."  
            else do 
              let timeInt = read time :: Int
              let categories = map (T.pack) $ tail components
              let view options = do
                      ~(vote, done) <- I.row do
                        vote <- I.button ButtonPrimary "Vote"
                        done <- I.button ButtonPrimary "Done"
                        pure (vote, done)
                      s <- I.select options
                      pure (vote, done, s)
              let initialState = PollViewState categories (Just (head categories)) []
              s <- P.evalState initialState $ 
                I.runView (view categories) (tell ctx) \(vote, done, s) -> do
                  when vote do
                      sel <- P.gets (^. #selected)
                      votes <- P.gets (^. #voted)
                      P.modify' (#voted .~ (votes ++ [fromMaybe (head categories) sel]))
                      I.replaceView (view categories) (void . I.edit)
                  when done do
                      finalSelected <- P.gets (^. #selected)
                      votes <- P.gets (^. #voted)
                      I.endView finalSelected
                      I.deleteInitialMsg
                      -- Logic to view votes and result nicely here
                      let countedVotes = countVotes (map T.unpack votes) []
                      void . I.respond $ show countedVotes -- TODO fix this
                      void . tell @T.Text ctx $ T.pack $ show votes 
                      pure ()

                  case s of
                    --s' -> do
                    --  void I.deferComponent
                    
                    Just s' -> do
                      P.modify' (#selected ?~ s')
                      void I.deferComponent
                    Nothing -> pure ()
              P.embed $ print s
              pure ()


