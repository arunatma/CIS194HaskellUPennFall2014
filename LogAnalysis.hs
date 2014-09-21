{-# OPTIONS_GHC -Wall #-}
{-
Name: Arunram A
This is the solutions file for: (Week 3)
http://www.seas.upenn.edu/~cis194/hw/03-ADTs.pdf
Also available at https://www.dropbox.com/s/gfpf4983y4mw2m5/03-ADTs.pdf?dl=0
-}

module LogAnalysis where

import Log
import Data.List
import Data.Char

parseMessage :: String -> MaybeLogMessage
parseMessage x = processWords $ words x

processWords :: [String] -> MaybeLogMessage
processWords (mt:ts:msg) 
    | mt == "I" = ValidLM (LogMessage Info (read ts :: TimeStamp) (unwords msg))
    | mt == "W" = ValidLM (LogMessage Warning (read ts :: TimeStamp) (unwords msg))
processWords (mt:sv:ts:msg) 
    | mt == "E" = ValidLM (LogMessage (Error (read sv :: Int)) (read ts :: TimeStamp) (unwords msg))
processWords xs = InvalidLM (unwords xs)
-- Test Cases
-- parseMessage "E 2 562 help help"
--  == ValidLM (LogMessage (Error 2) 562 "help help")
-- parseMessage "I 29 la la la"
--  == ValidLM (LogMessage Info 29 "la la la")
-- parseMessage "This is not in the right format"
--  == InvalidLM "This is not in the right format"


validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (x:xs) = case x of
    ValidLM lm   -> lm : validMessagesOnly xs 
    InvalidLM _ -> validMessagesOnly xs 
    
parse :: String -> [LogMessage]
parse x = validMessagesOnly $ map parseMessage (lines x)
-- try testParse parse 10 "error.log" in ghci

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage Info t _)     = t
timeStamp (LogMessage Warning t _)  = t
timeStamp (LogMessage (Error _) t _) = t

msgText :: LogMessage -> String
msgText (LogMessage Info _ msg)     = msg
msgText (LogMessage Warning _ msg)  = msg
msgText (LogMessage (Error _) _ msg) = msg

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs x y = compare (timeStamp x) (timeStamp y)

-- Test Cases
-- compareMsgs (LogMessage Warning 153 "Not a speck of light is showing, 
--                  so the danger must be growing...")
--             (LogMessage Info 208 "the Weighted Companion Cube cannot talk")
--              == LT
-- compareMsgs (LogMessage (Error 101) 2001 "My God! It’s full of stars!")
--             (LogMessage Info 2001 "Daisy, Daisy, give me your answer do.")
--              == EQ

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy (compareMsgs)

-- extract only the error messages with severity of at least 50
processWrongLogs :: [LogMessage] -> [String]
processWrongLogs [] = []
processWrongLogs (x:xs) 
    | whatsWrong x  = (msgText x) : processWrongLogs xs
    | otherwise     = processWrongLogs xs 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = processWrongLogs (sortMessages xs)

whatsWrong :: LogMessage -> Bool
whatsWrong (LogMessage Info _ _) = False
whatsWrong (LogMessage Warning _ _) = False
whatsWrong (LogMessage (Error s) _ _) = if s >= 50 then True else False
-- try giving    testWhatWentWrong  parse whatWentWrong "error.log" in ghci

-- filter out LogMessages containing the given string
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _ [] = []
messagesAbout t (x:xs)
    | stringExists t x  = x : messagesAbout t xs
    | otherwise         = messagesAbout t xs
    
stringExists :: String -> LogMessage -> Bool
stringExists t x = isInfixOf (map toLower t) (map toLower (msgText x))    
    
-- filter out LogMessages containing the given string 
-- or logged with high severity    
whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced _ [] = [] 
whatWentWrongEnhanced t (x:xs) 
    | combinedPredicate t x = (msgText x) : (whatWentWrongEnhanced t xs)
    | otherwise             = whatWentWrongEnhanced t xs
    where combinedPredicate t' x' = (|||) whatsWrong (stringExists t') x'
-- try the following command on ghci
-- testWhatWentWrong parse (whatWentWrongEnhanced "relish") "error.log"

-- function that combines both conditions using logical OR (||) 
(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x

