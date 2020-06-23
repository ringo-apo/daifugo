{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import System.Random.Shuffle (shuffleM)

-- (5'') 勝敗の判定をしよう

data Suit = Spade | Club | Diamond | Heart deriving (Eq, Enum)
data Number = N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | N11 | N12 | N13 | N1 | N2 deriving (Eq, Ord, Enum)
data JokerNumber = J1 | J2 deriving (Eq, Ord, Enum, Show)
data Card = Card Suit Number | Joker JokerNumber deriving Eq

allCards :: [Card]
allCards = [Card s n | s <- [Spade .. Heart], n <- [N3 .. N2]] ++ [Joker j | j <- [J1,J2]]

isNormalCard :: Card -> Bool
isNormalCard (Card _ _) = True
isNormalCard _ = False

isJoker :: Card -> Bool
isJoker (Card _ _) = False
isJoker _ = True

sameNumber :: Card -> Card -> Bool
sameNumber (Card _ k) (Card _ l) = k == l
sameNumber (Joker k) (Joker l) = k == l
sameNumber _ _ = False

toNumber :: Int -> Number
toNumber n
  | 3 <= n && n <= 13 = toEnum $ n - 3
  | n == 1 || n == 2 = toEnum $ n + 10
  | otherwise = toNumber (n `mod` 13)

fromNumber :: Number -> Int
fromNumber n
  | n <= N13 = fromEnum n + 3
  | otherwise = fromEnum n - 10

instance Show Suit where
  show Spade = "S"
  show Club = "C"
  show Diamond = "D"
  show Heart = "H"

instance Show Number where
  show N1 = "A"
  show N11 = "J"
  show N12 = "Q"
  show N13 = "K"
  show k = show $ fromNumber k

instance Show Card where
  show (Card s n) = show s ++ show n
  show (Joker k) = "Joker " ++ show (fromEnum k)

instance Ord Card where
  Joker _ <= Joker _ = False
  Card _ _ <= Joker _ = True
  Joker _ <= Card _ _ = False
  Card _ k <= Card _ k' = k <= k'

deal :: Int -> IO [[Card]]
deal n = fmap (fmap sort . take n . chunksOf (length allCards `div` n)) $ shuffleM allCards

data Game = Game {
  _decks :: IM.IntMap [Card],
  _turn :: Int,
  _players :: Int,
  _layout :: Maybe [Card],
  _passtimes :: Int,
  _winner :: [Int]
  }

makeLenses ''Game

strongPairs :: [Card] -> [Card] -> [[Card]]
strongPairs lay hands = concat $ fmap combi $ filter (\h -> h !! 0 > lay !! 0) $ filter (\h -> length h >= length lay) hands' where
  hands' = groupBy sameNumber hands
  combi us = filter (\h -> length h == length lay) $ subsequences us

game :: StateT Game IO ()
game = do
  t <- use turn
  ws <- use winner

  when (t `notElem` ws) $ do
    k <- case t == 0 of
      True -> (lift $ putStrLn $ "あなたの番です.") >> runExceptT player
      False -> (lift $ putStrLn $ "CPU" ++ show t ++ "さんの番です.") >> runExceptT auto

    case k of
      Left () -> do
        lift $ putStrLn $ "パス"
        passtimes += 1
      Right l -> do
        lift $ putStrLn $ "場札:" ++ show l
        layout .= Just l
        passtimes .= 0

  p <- use players
  ws <- use winner
  pt <- use passtimes
  when (pt >= p - length ws) $ do
    lift $ putStrLn $ "場が流れました."
    layout .= Nothing
    turn -= 1

  ds <- use (decks . ix t)
  ws <- use winner
  when (length ds == 0 && t `notElem` ws) $ do
    winner %= (t :)
    case t == 0 of
      True -> lift $ putStrLn $ "あなたの勝ち抜けです."
      False -> lift $ putStrLn $ "CPU" ++ show t ++ "さんが勝ち抜けました."

  p <- use players
  turn %= nextTurn p

  ws <- use winner
  when (length ws < p) $ game

  where
    nextTurn p t | t == p-1 = 0
                 | otherwise = t+1

player :: ExceptT () (StateT Game IO) [Card]
player = do
  ds <- use (decks . ix 0)
  ls <- use layout

  lift $ lift $ putStrLn $ "手札: " ++ show (groupBy sameNumber ds)
  ps <- case (\l -> strongPairs l ds) <$> ls of
    Just ps -> (lift $ lift $ putStrLn $ "候補: " ++ show ps) >> return ps
    Nothing -> return $ concat $ fmap subsequences $ groupBy sameNumber ds

  when (length ps == 0) $ throwError ()

  lift $ lift $ putStrLn "出すカードの数字を入力してください. (J: ジョーカー, P: パス)"
  str <- lift $ lift getLine
  when (str == "P") $ throwError ()

  case contains str ps of
    Just ps' -> lift $ process ds ps'
    Nothing -> do
      lift $ lift $ putStrLn "正しい数字を入力してください."
      player

  where
    process :: [Card] -> [[Card]] -> StateT Game IO [Card]
    process ds ps = do
      case length ps == 1 of
        True -> do
          decks %= IM.insert 0 (ds \\ ps !! 0)
          return $ ps !! 0
        False -> whichToDiscard ds ps

    whichToDiscard :: [Card] -> [[Card]] -> StateT Game IO [Card]
    whichToDiscard ds ps = do
      let psi = zip [0..] ps
      lift $ putStrLn $ unwords $ fmap (\(i,c) -> show i ++ ":" ++ show c) psi
      lift $ putStrLn "どのカードを出しますか?"
      str <- lift getLine
      case pick str ps of
        Just cs -> do
          decks %= IM.insert 0 (ds \\ cs)
          return cs
        Nothing -> whichToDiscard ds ps

    contains :: String -> [[Card]] -> Maybe [[Card]]
    contains "J" ds = Just $ filter (any isJoker) ds
    contains str ds = isEmpty $ filter (any (isSameNumber (read str))) <$> check where
      check = if all isDigit str && 1 <= read str && read str <= 13
              then Just ds else Nothing
      isEmpty p = if p == Just [] then Nothing else p

    pick :: String -> [[Card]] -> Maybe [Card]
    pick ss ps = (!! read ss) <$> check where
      check = if (all isDigit) ss &&
                 (0 <= read ss && read ss <= length ps - 1)
              then Just ps else Nothing

    isSameNumber :: Int -> Card -> Bool
    isSameNumber n x = isNormalCard x && cardNumber x == n where
      cardNumber (Card _ n) = fromNumber n
      cardNumber _ = -1

auto :: ExceptT () (StateT Game IO) [Card]
auto = do
  t <- use turn
  ls <- use layout
  ds <- use (decks . ix t)

  case (\l -> strongPairs l ds) <$> ls of
    Just [] -> throwError ()
    Just ps -> do
      let ps' = ps !! 0
      decks %= IM.insert t (ds \\ ps')
      return ps'
    Nothing -> do
      let ps' = (groupBy sameNumber ds) !! 0
      decks %= IM.insert t (ds \\ ps')
      return ps'

main = do
  putStrLn "Let's play 大富豪!"

  let pl = 6
  d <- deal pl
  g <- execStateT game $ Game (IM.fromList $ zip [0..pl-1] d) 0 pl Nothing 0 []

  let ws = zip [1..] (reverse $ g^.winner)
  putStrLn $ "\n=========="
  putStrLn $ "勝敗"
  forM_ ws $ \(i,k) -> do
    putStrLn $ show i ++ "位: " ++ (if k == 0 then "あなた" else ("CPU" ++ show k))
