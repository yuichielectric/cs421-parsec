module Tests where

import MyParsec

allTests :: [([Bool], String)]
allTests = [
    (tests_char, "char tests")
    , (tests_string, "string tests")
    , (tests_letter, "letter tests")
    , (tests_digit, "digit tests")
    , (tests_whiteSpace, "whiteSpace tests")
    , (tests_whiteSpace1, "whiteSpace1 tests")
    , (tests_integer, "integer tests")
    , (tests_string_literal, "string_literal tests")
    , (tests_many, "many tests")
    , (tests_many1, "many1 tests")
    , (tests_identifier, "identifier tests")
    , (tests_letExpr, "letExpr tests")
    ]


do_parse p input = parse p (State input (Pos 1 0))

tests_char :: [Bool]
tests_char = [
    do_parse (char ' ') " " == (Consumed (Ok ' ' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse (char 'a') "abc" == (Consumed (Ok 'a' (State "bc" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse (char 'a') "def" == (Empty (Error (Message (Pos 1 1) "d" ["'a'"])))]

tests_string = [
    do_parse (string "abc") "abc" == (Consumed (Ok () (State "" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse (string "abc") "abc def" == (Consumed (Ok () (State " def" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse (string "abc") "" == (Empty (Error (Message (Pos 1 0) "end of input" ["'a'"])))
    , do_parse (string "abc") "a" == (Consumed (Error (Message (Pos 1 1) "end of input" ["'b'"])))]

tests_letter = [
    do_parse letter "a" == (Consumed (Ok 'a' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse letter "abc" == (Consumed (Ok 'a' (State "bc" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse letter "" == (Empty (Error (Message (Pos 1 0) "end of input" ["letter"])))
    , do_parse letter "*" == (Empty (Error (Message (Pos 1 1) "*" ["letter"])))]

tests_digit = [
    do_parse digit "1" == (Consumed (Ok '1' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse digit "123" == (Consumed (Ok '1' (State "23" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse digit "" == (Empty (Error (Message (Pos 1 0) "end of input" ["digit"])))
    , do_parse digit "a" == (Empty (Error (Message (Pos 1 1) "a" ["digit"])))
    , do_parse digit "*" == (Empty (Error (Message (Pos 1 1) "*" ["digit"])))]

tests_whiteSpace = [
    do_parse whiteSpace " " == (Consumed (Ok () (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))
    , do_parse whiteSpace "a" == (Empty (Ok () (State "a" (Pos 1 0)) (Message (Pos 1 0) "" [])))
    , do_parse whiteSpace "" == (Empty (Ok () (State "" (Pos 1 0)) (Message (Pos 1 0) "" [])))]

tests_whiteSpace1 = [
    do_parse whiteSpace1 " " == (Consumed (Ok () (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))
    , do_parse whiteSpace1 "a" == (Empty (Error (Message (Pos 1 1) "a" ["' '"])))
    , do_parse whiteSpace1 "" == (Empty (Error (Message (Pos 1 0) "end of input" ["' '"])))]

tests_integer = [
    do_parse integer "1" == (Consumed (Ok "1" (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))
    , do_parse integer "123" == (Consumed (Ok "123" (State "" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse integer "123 abc" == (Consumed (Ok "123" (State " abc" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse integer "abc" == (Empty (Error (Message (Pos 1 1) "a" ["integer"])))]

tests_string_literal = [
    do_parse string_literal "\"\"" == (Consumed (Ok "" (State "" (Pos 1 2)) (Message (Pos 1 2) "" [])))
    , do_parse string_literal "\"hello\"" == (Consumed (Ok "hello" (State "" (Pos 1 7)) (Message (Pos 1 7) "" [])))
    , do_parse string_literal "123" == (Empty (Error (Message (Pos 1 1) "1" ["string_literal"])))]

tests_many = [
    do_parse (do MyParsec.many (char 'a')) "a" == (Consumed (Ok "a" (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))
    , do_parse (do MyParsec.many (char 'a')) "aaa" == (Consumed (Ok "aaa" (State "" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse (do MyParsec.many (char 'a')) "aab" == (Consumed (Ok "aa" (State "b" (Pos 1 2)) (Message (Pos 1 2) "" [])))
    , do_parse (do MyParsec.many (char 'a')) "def" == (Empty (Ok "" (State "def" (Pos 1 0)) (Message (Pos 1 0) "" ["'a'"])))
    , do_parse (do MyParsec.many (char 'a')) "" == (Empty (Ok "" (State "" (Pos 1 0)) (Message (Pos 1 0) "" ["'a'"])))]

tests_many1 = [
    do_parse (do many1 (char 'a')) "a" == (Consumed (Ok "a" (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))
    , do_parse (do many1 (char 'a')) "aaa" == (Consumed (Ok "aaa" (State "" (Pos 1 3)) (Message (Pos 1 3) "" [])))
    , do_parse (do many1 (char 'a')) "aab" == (Consumed (Ok "aa" (State "b" (Pos 1 2)) (Message (Pos 1 2) "" [])))
    , do_parse (do many1 (char 'a')) "def" == (Empty (Error (Message (Pos 1 1) "d" ["'a'"])))
    , do_parse (do many1 (char 'a')) "" == (Empty (Error (Message (Pos 1 0) "end of input" ["'a'"])))]

tests_identifier = [
    do_parse identifier "CamelCase" == (Consumed (Ok "CamelCase" (State "" (Pos 1 9)) (Message (Pos 1 9) "" [])))
    , do_parse identifier "snake_case" == (Consumed (Ok "snake_case" (State "" (Pos 1 10)) (Message (Pos 1 10) "" [])))
    , do_parse identifier "CamelCase snake_case" == (Consumed (Ok "CamelCase" (State " snake_case" (Pos 1 9)) (Message (Pos 1 9) "" [])))
    , do_parse identifier "" == (Empty (Error (Message (Pos 1 0) "end of input" ["identifier"])))
    , do_parse identifier "***" == (Empty (Error (Message (Pos 1 1) "*" ["identifier"])))]

tests_letExpr = []