module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative(..))

newtype Parser a = Parser (String -> Consumed a)

data Consumed a = Consumed (Reply a) | Empty (Reply a)
    deriving (Eq, Show)

data Reply a = Ok a String | Error
    deriving (Eq, Show)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser (\input -> Empty (Ok a input))
    (<*>) = ap

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f  = Parser (\input -> case parse p input of
                            Empty reply1
                                -> case (reply1) of
                                    Ok x rest -> (parse (f x) rest)
                                    Error     -> Empty Error
                            Consumed reply1
                                -> Consumed
                                    (case (reply1) of
                                        Ok x rest
                                            -> case (parse (f x) rest) of
                                                Consumed reply2 -> reply2
                                                Empty reply2    -> reply2
                                        Error -> Error
                                        ))

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = Parser (\cs -> Empty Error)
    p `mplus` q =
        Parser (\input ->
            case parse p input of
                Empty Error -> parse q input
                Empty ok    -> case parse q input of
                                Empty _  -> Empty ok
                                consumed -> consumed
                consumed    -> consumed)

-- Apply parser
parse :: Parser a -> String -> Consumed a
parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> Empty Error
                        (c:cs) -> Consumed (Ok c cs))

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do {char c; string cs}

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- (many1 p <|> return []); return (a:as)}

letter :: Parser Char
letter = sat isAlpha

digit :: Parser Char
digit = sat isDigit

whiteSpace :: Parser ()
whiteSpace = do {Parsing.many (char ' '); return ()}

letExpr :: Parser String
letExpr = do {identifier; whiteSpace; char '='; whiteSpace; expr}

identifier = many1 (letter <|> digit <|> char '_')


try :: Parser a -> Parser a
try p = Parser (\input -> case parse p input of
                    Consumed Error -> Empty Error
                    other          -> other)

expr = do {string "let"; whiteSpace; letExpr } <|> identifier
