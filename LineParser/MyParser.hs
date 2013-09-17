module MyParser (MyParser, parse, (<|>), satisfy, eof, many, many1) where

import Control.Monad
import Data.Either
import Data.Maybe

data MyParser tok a = MyParser ([tok] -> Maybe (a,[tok]))

parse :: MyParser tok a -> [tok] -> Either String a
parse p input = case run p input of
                  Just (x, _) -> Right x
                  Nothing     -> Left "parse error"

run :: MyParser tok a -> [tok] -> Maybe (a,[tok])
run (MyParser f) input = f input

instance Monad (MyParser tok) where
  return x  = MyParser (\input -> Just (x, input))
  p >>= f   = MyParser (\input -> case run p input of
                                    Just (x, input') -> run (f x) input'
                                    Nothing          -> Nothing)
  fail msg  = MyParser (\input -> Nothing)

infixr 1 <|>

(<|>) :: MyParser tok a -> MyParser tok a -> MyParser tok a
p1 <|> p2 = MyParser nextState
  where
    nextState input = case run p1 input of
                        Just x  -> Just x
                        Nothing -> run p2 input

satisfy :: (tok -> Bool) -> MyParser tok tok
satisfy f = MyParser nextState
  where
    nextState (x:xs) | f x = Just (x, xs)
    nextState _            = Nothing

eof :: MyParser tok ()
eof = MyParser nextState
  where
    nextState [] = Just ((), [])
    nextState _  = Nothing

many :: MyParser tok a -> MyParser tok [a]
many p =  do x <- p
             xs <- many p
             return (x:xs)
      <|> return []

many1 :: MyParser tok a -> MyParser tok [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)
