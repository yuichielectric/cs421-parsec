module Parser where

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
    p `mplus` q = Parser (\cs -> case parse p cs of
                                    [] -> parse q cs
                                    [(a, cs')] -> [(a, cs')])

-- Apply parser
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p


item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c,cs)])

p :: Parser (Char, Char)
p = do {c <- item; item; d <- item; return (c, d)}

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- Parser.many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) `mplus` return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do {a <- p; as <- Parser.many (do {sep; p}); return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `mplus` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = (do f <- op
                       b <- p
                       rest (f a b))
                   `mplus` return a

-- Lexical combinators

space :: Parser String
space = Parser.many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symbol :: String -> Parser String
symbol cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

-- Example: arithmetic expressions

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit `mplus` do {symbol "("; n <- expr; symbol ")"; return n}

digit :: Parser Int
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}

addop :: Parser (Int -> Int -> Int)
addop = do {symbol "+"; return (+)} `mplus` do {symbol "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symbol "*"; return (*)} `mplus` do {symbol "/"; return (div)}
