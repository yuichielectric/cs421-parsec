module MyParsec where

import Data.Char
import Data.List (intercalate)
import Control.Monad
import Control.Applicative (Alternative(..))

newtype Parser a = Parser (State -> Consumed a)

-- State has the following fields:
--  * Rest of the input string
--  * Current position
data State = State String Pos
    deriving (Eq, Show)

-- Message has the following fields:
--  * Position of the error
--  * Unexpected input
--  * List of expected productions
data Message = Message Pos String [String]
    deriving (Eq, Show)

data Consumed a = Consumed (Reply a) | Empty (Reply a)
    deriving (Eq, Show)

data Reply a = Ok a State Message | Error Message
    deriving (Eq, Show)

data Pos = Pos {line :: Int, column :: Int}
    deriving (Eq)

instance Show Pos where
    show (Pos l c) = "(line " ++ show l ++ ", column " ++ show c ++ ")"

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser (\state ->
                        Empty (Ok a state (Message (pos state) [] [])))
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

-----------------------
-- Basic combinators --
-----------------------

instance Monad Parser where
    p >>= f = Parser (\state -> case parse p state of
                            Empty reply1
                                -> case reply1 of
                                    Ok x state' msg -> parse (f x) state'
                                    Error msg       -> Empty (Error msg)
                            Consumed reply1
                                -> Consumed
                                    (case reply1 of
                                        Ok x state' msg
                                            -> case parse (f x) state' of
                                                Consumed (Ok b state'' msg') -> Ok b state'' (merge msg msg')
                                                Consumed (Error msg') -> Error (merge msg msg')
                                                Empty (Ok b state'' msg') -> Ok b state'' (merge msg msg')
                                                Empty (Error msg') -> Error (merge msg msg')
                                        Error msg -> Error msg
                                        ))

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
merge (Message pos inp exp1) (Message newPos newInp exp2) = Message newPos newInp (exp1 ++ exp2)

parse :: Parser a -> State -> Consumed a
parse (Parser p) = p

pos :: State -> Pos
pos (State _ pos) = pos

sat :: (Char -> Bool) -> Parser Char
sat test = Parser (\(State input pos) ->
    case input of
        "" -> Empty (Error (Message pos "end of input" []))
        (c:cs) ->
            let newPos = nextPos c pos
                newState = State cs newPos
            in if test c
                then seq newPos (Consumed (Ok c newState (Message pos [] [])))
                else Empty (Error (Message newPos [c] [])))

-- Specify the expected production
(<?>) :: Parser a -> String -> Parser a
p <?> exp = Parser(
    \state ->
        case (parse p state) of
            Empty (Error msg)   -> Empty (Error (expect msg exp))
            Empty (Ok x st msg) -> Empty (Ok x st (expect msg exp))
            other               -> other)

expect (Message pos inp _) exp = Message pos inp ([exp])

nextPos :: Char -> Pos -> Pos
nextPos '\n' (Pos l _) = Pos (l + 1) 0
nextPos _ (Pos l c) = Pos l (c + 1)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- (many1 p <|> return []); return (a:as)}

try :: Parser a -> Parser a
try p = Parser (\input -> case parse p input of
                    Consumed (Error msg) -> Empty (Error msg)
                    other          -> other)

-----------------------
-- Primitive parsers --
-----------------------

char :: Char -> Parser Char
char c = sat (c ==) <?> (show c)

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do {char c; string cs}

letter :: Parser Char
letter = sat isAlpha <?> "letter"

digit :: Parser Char
digit = (sat isDigit) <?> "digit"

whiteSpace :: Parser ()
whiteSpace = do {MyParsec.many (char ' '); return ()}

whiteSpace1 :: Parser ()
whiteSpace1 = do {many1 (char ' '); return ()}

integer :: Parser String
integer = many1 digit <?> "integer"

---------------------
-- Complex parsers --
---------------------

identifier :: Parser String
identifier = many1 (letter <|> digit <|> char '_') <?> "identifier"

letExpr :: Parser String
letExpr = do {identifier; whiteSpace; char '='; whiteSpace; expr}

expr = do {string "let"; whiteSpace1; letExpr } <|> identifier

expr2 = try (do { string "let"; whiteSpace1; letExpr})
        <|> identifier

-------------------
-- Parser runner --
-------------------

-- Run a parser and prettyprint the result
run :: Parser a -> String -> IO ()
run p input =
    case parse p (State input (Pos 1 0)) of
        Consumed (Ok result state message) -> showSuccess state
        Empty (Ok result state message)    -> showSuccess state
        Consumed (Error (Message pos inp exp)) -> showError pos inp exp
        Empty (Error (Message pos inp exp))    -> showError pos inp exp

showSuccess :: State -> IO ()
showSuccess (State rest pos) = do
    putStrLn $ "Successfully parsed to " ++ show pos
    putStrLn $ "Rest of input: \"" ++ rest ++ "\""

showError :: Pos -> String -> [String] -> IO ()
showError pos inp exp = do
    putStrLn $ "Parse error at " ++ show pos
    putStrLn $ "unexpected \"" ++ inp ++ "\""
    putStrLn $ "expecting " ++ (intercalate ", " exp)
