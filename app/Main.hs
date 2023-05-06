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


tally :: [RawEmoji] -> [T.Text] -> [Reaction] -> [(Integer, RawEmoji, T.Text)]
tally emojies categories reactions = [(view #count react, emoji', category) | (emoji', category) <- contestants, react <- reactions, view #emoji react == emoji', view #me react] 
                                      where
                                        contestants = zip emojies categories

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

--data Change = Move | Stay 
data Status = Muted | Unmuted 

instance D.Default Status where
  def = Unmuted

changeMute :: P.Members '[P.State Status, Error Status] r => P.Sem r ()
changeMute = do
  s <- P.get
  case s of
    Muted -> P.put Unmuted
    Unmuted -> P.put Muted

getMute :: P.Members '[P.State Status, Error Status] r => P.Sem r Status
getMute = do P.get

data SState s m a where
  SGet :: SState s m a
  SPut :: s -> SState s m ()

P.makeSem ''SState

sstateTransform :: (P.Members '[P.State s] r) => P.Sem (SState s ': r) a -> P.Sem r a
sstateTransform = P.interpret \case
  --SGet          -> P.get
  SPut newState -> P.put newState


data MuteState m a where
  MGet :: MuteState m a
  MPut :: a -> MuteState m ()

P.makeSem ''MuteState



transformMute :: (P.Members '[P.State Status] r) => P.Sem (MuteState ': r) a -> P.Sem r a
transformMute = P.interpret \case
  MGet -> undefined
  MPut newState -> undefined --P.raise $ P.put newState

transposeMute :: (P.Member (P.State Status) r) => P.Sem (MuteState ': r) a -> P.Sem r a
transposeMute = P.interpret \case
  MGet -> undefined
  MPut newState -> undefined -- P.put newState --undefined

--stateToBotc :: P.Member BotC r => P.Sem (SState ': r) a -> P.Sem r a
--stateToBotc = undefined

--data Silence m a where
--  ChangeStatus :: Change -> Silence m ()
--  GetStatus :: Silence m Status
--  NextStatus :: Silence m Status
--
--data SilenceState s m a where
--  SetState :: s -> SilenceState s m ()
--  CurState :: SilenceState s m a 
--
--
--P.makeSem ''SilenceState
--
--
--silenceInterpret :: (P.Member (P.State s) r, P.Member (P.Embed IO) r) => P.Sem (SilenceState s ': r) a -> P.Sem r a
--silenceInterpret = P.interpret \case
--  SetState s -> P.put s 
--
--silenceInterpret2 :: (P.Member (P.State s) r, P.Member (P.Embed IO) r) => P.Sem (SilenceState s ': r) a -> P.Sem r a
--silenceInterpret2 :: (P.Member (P.Embed IO) r) => P.Sem (SilenceState s ': r) a -> P.State s r a
--silenceInterpret2 = P.interpret \case
  --CurState -> P.Get



--moveStatus :: Change -> Status -> Status
--moveStatus Stay x = x
--moveStatus Move Muted = Unmuted
--moveStatus Move Unmuted = Muted
--
--P.makeSem ''Silence
--
--silenceToBot :: P.Member (P.Embed s) r => P.Sem (Silence ': r) a -> P.Sem r a
--silenceToBot = P.interpret \case
--  ChangeStatus change -> undefined
--  GetStatus           -> undefined
--  NextStatus          -> undefined --P.mod $ moveStatus Move $ getStatus




-- move :: Change -> Member (Silence ': r) a -> P.Sem 

--changeSilence :: (P.Member Silence r) => P.Sem (Silence ': r) a -> P.Sem r a
--changeSilence x = P.interpret case x of 
--                    ChangeStatus change -> moveStatus change Muted



--changeState :: (P.Member Silence r) => Change -> P.Sem r ()
--changeState change = do
  --status <- P.get
--  let status = Unmuted
--  let newStatus = moveStatus change status
--  P.put (status)

--changeState :: Change -> P.Sem m ()
--changeState Stay = P.put ()
--changeState Move = do
--  status <- P.get
--  let newStatus = moveStatus Move status
--  P.put newStatus



-- type Silence Status
--data SilenceState status change = SilenceState
--  { status :: Status
--  , change :: Change
--  }
--
--
--P.makeSem ''SilenceState
--instance D.Default Change where
--  def = Stay
--instance D.Default Status where
--  def = Unmuted
-- 
--type Mute s a = P.State (s, a) Change
--
--changeStatus :: Change -> Status -> Status
--changeStatus Stay x = x
--changeStatus Move Muted = Unmuted
--changeStatus Move Unmuted = Muted
--
--changeStatus' :: P.Members [Change] 
--
--changeSilence :: (Member (P.State SilenceState) r) => P.Sem r ()
--changeSilence s = P.modify (\x -> x {Main.status = changeStatus s})

--silence :: MonadState (s, Change) m => Mute s Change -> m ()
--silence s = do
  --(currentState, currentMove) <- P.Get
  --undefined
declareState :: P.Members '[P.State Status, Error Status] r => P.Sem r ()
declareState = P.put def

runState :: P.Member (P.State Status) r => P.Sem r a
runState = do
  runState
  

padCategories :: [T.Text] -> [T.Text]
padCategories words = map T.pack $ padCategoriesHelper (map T.unpack words) 0

padCategoriesHelper :: [String] -> Int -> [String]
padCategoriesHelper [] len = []
padCategoriesHelper words len
  | all ((==len) . length) words = words
  | any ((> len) . length) words = padCategoriesHelper words (foldl max 0 $ map length words)
  | otherwise = [word ++ ['-' | _ <- [0..((+1) $ len - length word)]] | word <- words]

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
            void $ DiP.info $ show ctx
            let emptyOption = SelectOption "" "" Nothing Nothing False
            let options' = [emptyOption & #label .~ T.pack "Test1" & #default_ .~ True, emptyOption & #label .~ T.pack "Test2"] :: [SelectOption]
            let newMessage = intoMsg (DT.pack "Apple") -- <> intoMsg ((Select options' Nothing Nothing Nothing False (CustomID "Bot 13")) :: Select) 
            void . DiP.info $ T.pack "Trying to send message"
            void . DiP.info $ T.pack $ show $ runToMessage newMessage
            void $ tell ctx $ runToMessage newMessage -- :: (BotC r, z) => PU.Member r z 
            let msg = ctxMessage ctx
            
            void $ DiP.info $ T.pack $ show msg
            void $ DiP.info @T.Text "Now printing the thing:"
            void $ updateMessage (getID $ ctxMessage ctx) (\x -> x {reactions = [Reaction 1 True $ UnicodeEmoji $ T.pack ":one:"]})
            let reacts = HTTPC.GetReactions (ctxChannelID ctx) (ctxMessage ctx)
            void $ tell @T.Text ctx "text"
 
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

          command @'[Integer, T.Text] "experiment" \ctx sleep arg -> do
            let content = T.unpack $ view #content $ ctxMessage ctx
            let categories = map T.pack $ drop 2 $ splitAtAll " " content
            -- Guild type has the following field that can be used later:
            -- emojis :: SnowflakeMap Emoji
            let emojies = take (length categories) defaultRawEmoji
            -- Construct message
            -- let messageToSend = intoMsg "Poll" <> intoMsg Embed (def & #description ?~ "Embed description")
            let embedded = (def :: Embed) & #title ?~ "Poll" & #description ?~ combineTextEmoji (padCategories categories) emojies


            sentMessage <- tell ctx $ intoMsg embedded <> intoMsg @T.Text "Polled"
            -- sentMessage <- tell @T.Text ctx $ T.pack $ show categories ++ "\nEnds after " ++ show sleep ++ " seconds"
            case sentMessage of 
              Right s -> do
                          -- void $ I.edit (void s) $ updateMessage (view #id s) (\x -> x {reactions = map (Reaction 1 True) emojies})
                          void . DiP.info . T.pack $ "Trying to rect to messageID " ++ show (view #id s) ++ " in channelID " ++ show (view #channelID s)
                          let requests = map (HTTPC.CreateReaction s s) emojies --(view #channelID s) (view #id s)) emojies 
                          mapM_ invoke requests -- React to own message to create "vote" buttons
                          void . DiP.alert @T.Text $ "Now trying simple react"
                          void $ invoke $ HTTPC.CreateReaction (view #channel ctx) (view #message ctx) $ UnicodeEmoji ":zero:"

                          liftIO $ delay (sleep * 1_000_000)
                          reactions <- getReactions s :: (BotC r) => P.Sem r [Reaction]
                          --reactions <- getMessage (fromMaybe tempSnowflake $ Cont.messageID s)
                          void $ tell @T.Text ctx $ T.pack $ "Reactions " <> show (length reactions) <> " "
                          void . tell @T.Text ctx . T.pack $ show reactions
                          -- Tally votes
                          -- TODO move to function
                          void . tell @T.Text ctx $ handlePollResults emojies categories reactions
                          
                
          react @'MessageCreateEvt \(msg, _usr, _member) -> do
            when (T.isInfixOf "haskell" $ view #content msg) do
              void . invoke $ CreateReaction msg msg (UnicodeEmoji "1️⃣")
          
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


