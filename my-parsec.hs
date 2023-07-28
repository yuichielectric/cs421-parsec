module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative (Alternative(..))

newtype Parser a = Parser (State -> Consumed a)

data State = State String Pos
    deriving (Eq, Show)

data Message = Message Pos String [String]
    deriving (Eq, Show)

data Consumed a = Consumed (Reply a) | Empty (Reply a)
    deriving (Eq, Show)

data Reply a = Ok a State Message | Error Message
    deriving (Eq, Show)

data Pos = Pos {line :: Int, column :: Int}
    deriving (Eq)

instance Show Pos where
    show (Pos l c) = "line " ++ show l ++ ", column " ++ show c

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser (\state ->
                        Empty (Ok a state (Message (pos state) [] [])))
    (<*>) = ap

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\state -> case parse p state of
                            Empty reply1
                                -> case (reply1) of
                                    Ok x state' msg -> parse (f x) state'
                                    Error msg       -> Empty (Error msg)
                            Consumed reply1
                                -> Consumed
                                    (case (reply1) of
                                        Ok x state' msg
                                            -> case parse (f x) state' of
                                                Consumed (Ok b state'' msg') -> Ok b state'' (merge msg msg')
                                                Consumed (Error msg') -> Error (merge msg msg')
                                                Empty (Ok b state'' msg') -> Ok b state'' (merge msg msg')
                                                Empty (Error msg') -> Error (merge msg msg')
                                        Error msg -> Error msg
                                        ))

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = Parser (\cs -> Empty (Error (Message (Pos 0 0) [] [])))
    p `mplus` q =
        Parser (\state ->
            case parse p state of
                Empty (Error msg1) -> case parse q state of
                                        Empty (Error msg2) -> mergeError msg1 msg2
                                        Empty (Ok x inp msg2) -> mergeOk x inp msg1 msg2
                                        consumed -> consumed
                Empty (Ok x inp msg1) -> case parse q state of
                                Empty (Error msg2)  -> mergeOk x inp msg1 msg2
                                Empty (Ok _ _ msg2) -> mergeOk x inp msg1 msg2
                                consumed -> consumed
                consumed    -> consumed)

mergeOk x inp msg1 msg2 = Empty (Ok x inp (merge msg1 msg2))
mergeError msg1 msg2 = Empty (Error (merge msg1 msg2))
merge (Message pos inp exp1) (Message _ _ exp2) = Message pos inp (exp1 ++ exp2)

-- Apply parser
parse :: Parser a -> State -> Consumed a
parse (Parser p) = p

pos :: State -> Pos
pos (State _ pos) = pos

-- item :: Parser Char
-- item = Parser (\cs -> case cs of
--                         State "" pos     -> Empty (Error (Message pos [] ["end of input"]))
--                         State (c:cs) pos -> Consumed (Ok c cs))

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do {c <- item; if p c then return c else mzero}

sat :: (Char -> Bool) -> Parser Char
sat test = Parser (\(State input pos) ->
    case input of
        "" -> Empty (Error (Message pos "end of input" []))
        (c:cs) -> if test c
                    then
                        let newPos = nextPos c pos
                            newState = State cs newPos
                        in seq newPos (Consumed (Ok c newState (Message pos [] [])))
                    else Empty (Error (Message pos [] ["unexpected " ++ [c]])))

-- sat :: (Char -> Bool) -> Parser Char
-- sat test = Parser (\(State input pos) ->
--     case input of
--         (c:cs) | test c
--             -> let newPos = nextPos c pos
--                    newState = State cs newPos
--                 in seq newPos
--                     (Consumed (Ok c newState (Message pos [] [])))
--         (c:cs) -> Empty (Error (Message pos [] ["unexpected " ++ [c]]))
--         []     -> Empty (Error (Message pos "end of input" [])))

nextPos :: Char -> Pos -> Pos
nextPos '\n' (Pos l _) = Pos (l + 1) 0
nextPos _ (Pos l c) = Pos l (c + 1)

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

whiteSpace1 = do {many1 (char ' '); return ()}

letExpr :: Parser String
letExpr = do {identifier; whiteSpace; char '='; whiteSpace; expr}

identifier = many1 (letter <|> digit <|> char '_')


try :: Parser a -> Parser a
try p = Parser (\input -> case parse p input of
                    Consumed (Error msg) -> Empty (Error msg)
                    other          -> other)

expr = do {string "let"; whiteSpace1; letExpr } <|> identifier


test = sat ('l' ==) >>= \_ ->
    sat ('n' ==)
