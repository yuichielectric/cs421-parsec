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
    ]


do_parse p input = parse p (State input (Pos 1 0))

tests_char :: [Bool]
tests_char = [
    do_parse (char ' ') " " == (Consumed (Ok ' ' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))
    , do_parse (char 'a') "abc" == (Consumed (Ok 'a' (State "bc" (Pos 1 1)) (Message (Pos 1 0) "" [])))]


tests_string = [
    do_parse (string "abc") "abc" == (Consumed (Ok () (State "" (Pos 1 3)) (Message (Pos 1 3) "" [])))]

tests_letter = [
    do_parse letter "a" == (Consumed (Ok 'a' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))]

tests_digit = [
    do_parse digit "1" == (Consumed (Ok '1' (State "" (Pos 1 1)) (Message (Pos 1 0) "" [])))]

tests_whiteSpace = [
    do_parse whiteSpace " " == (Consumed (Ok () (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))]

tests_whiteSpace1 = [
    do_parse whiteSpace1 " " == (Consumed (Ok () (State "" (Pos 1 1)) (Message (Pos 1 1) "" [])))]
