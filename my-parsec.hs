module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative(..))

type Parser a = String -> Consumed a

data Consumed a = Consumed (Reply a) | Empty (Reply a)
    deriving (Eq, Show)

data Reply a = Ok a String | Error
    deriving (Eq, Show)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = \input -> Empty (Ok x input)
    (<*>) = ap

instance Monad Parser where
    p >>= f  = \input -> case (p input) of
                            Empty reply1
                                -> case (reply1) of
                                    Ok x rest -> ((f x) rest)
                                    Error     -> Empty Error
                            Consumed reply1
                                -> Consumed
                                    (case (reply1) of
                                        Ok x rest
                                            -> case ((f x) rest) of
                                                Consumed reply2 -> reply2
                                                Empty reply2    -> reply2
                                        error -> error
                                        )

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    p `mplus` q =
        \input ->
            case (p input) of
                Empty Error -> (q input)
                Empty ok    -> case (q input) of
                                Empty _  -> Empty ok
                                consumed -> consumed
                consumed    -> consumed

-- Apply parser
parse :: Parser a -> String -> [(a, String)]
parse p = p

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do {char c; string cs}

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- (many1 p <|> return []); return (a:as)}

identifier = many1 (letter <|> digit <|> char '_')


try :: Parser a -> Parser a
try p = \input -> case (p input) of
                    Consumed Error -> Empty Error
                    other          -> other
