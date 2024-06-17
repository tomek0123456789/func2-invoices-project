module Ui.UiUtils (
    ParseErrorMessage (..),
    InvalidNumErrorMessage (..),
    InvalidNameErrorMessage (..),
    readIntWithError,
    readFloatWithError,
    readMoneyWithError,
    readPositiveIntWithError,
    readPositiveFloatWithError,
    readPositiveMoneyWithError,
    readValidatedIntWithError,
    readValidatedMoneyWithError,
    readDate,
    readDateAfter,
    readTin,
    readValidName,
    trim,
) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Data.Time.Calendar (Day, DayOfMonth, MonthOfYear, Year, fromGregorianValid)
import Models.Invoice (Money)
import System.IO
import Text.Read (readMaybe)

-- import Debug.Trace

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

newtype ParseErrorMessage = ParseErrorMessage {parseErrorMessage :: String}
newtype InvalidNumErrorMessage = InvalidNumErrorMessage {invalidNumErrorMessage :: String}
newtype InvalidNameErrorMessage = InvalidNameErrorMessage {invalidNameErrorMessage :: String}

-- todo do that generically
readIntWithError :: ParseErrorMessage -> IO Int
readIntWithError pem = do
    maybeInt <- getLine
    let parsedMaybeInt = readMaybe maybeInt :: Maybe Int
    case parsedMaybeInt of
        Just valid -> return valid
        Nothing -> do
            putStr (parseErrorMessage pem) >> hFlush stdout
            readIntWithError pem

readFloatWithError :: ParseErrorMessage -> IO Float
readFloatWithError pem = do
    maybeFloat <- getLine
    let parsedMaybeFloat = readMaybe maybeFloat :: Maybe Float
    case parsedMaybeFloat of
        Just valid -> return valid
        Nothing -> do
            putStr (parseErrorMessage pem) >> hFlush stdout
            readFloatWithError pem

readMoneyWithError :: ParseErrorMessage -> IO Money
readMoneyWithError pem = do
    maybeMoney <- getLine
    let parsedMaybeMoney = readMaybe maybeMoney :: Maybe Money
    case parsedMaybeMoney of
        Just valid -> return valid
        Nothing -> do
            putStr (parseErrorMessage pem) >> hFlush stdout
            readMoneyWithError pem

-- TODO refactor to accept a lambda and validate against that
readPositiveIntWithError :: ParseErrorMessage -> InvalidNumErrorMessage -> IO Int
readPositiveIntWithError = readValidatedIntWithError (> 0)

readPositiveMoneyWithError :: ParseErrorMessage -> InvalidNumErrorMessage -> IO Money
readPositiveMoneyWithError = readValidatedMoneyWithError (> 0)

readPositiveFloatWithError :: ParseErrorMessage -> InvalidNumErrorMessage -> IO Float
readPositiveFloatWithError pem inem = do
    parsedFloat <- readFloatWithError pem
    if parsedFloat <= 0
        then do
            putStr (invalidNumErrorMessage inem) >> hFlush stdout
            readPositiveFloatWithError pem inem
        else return parsedFloat

readValidatedIntWithError :: (Int -> Bool) -> ParseErrorMessage -> InvalidNumErrorMessage -> IO Int
readValidatedIntWithError fn pem inem = do
    parsedInteger <- readIntWithError pem
    if fn parsedInteger
        then return parsedInteger
        else do
            putStr (invalidNumErrorMessage inem) >> hFlush stdout
            readValidatedIntWithError fn pem inem

readValidatedMoneyWithError :: (Money -> Bool) -> ParseErrorMessage -> InvalidNumErrorMessage -> IO Money
readValidatedMoneyWithError fn pem inem = do
    parsedMoney <- readMoneyWithError pem
    if fn parsedMoney
        then return parsedMoney
        else do
            putStr (invalidNumErrorMessage inem) >> hFlush stdout
            readValidatedMoneyWithError fn pem inem

readDate :: IO Day -- accepts input in form of YYYY-MM-DD
readDate = do
    maybeValues <- map (\x -> readMaybe x :: Maybe Int) . splitOn "-" <$> getLine
    if invalidSplit maybeValues
        then do
            putStr "Invalid date format, should be YYYY-MM-DD" >> hFlush stdout
            readDate
        else do
            let values = map fromJust maybeValues
            let maybeDay = fromGregorianValid (toInteger (values !! 0) :: Year) ((values !! 1) :: MonthOfYear) ((values !! 2) :: DayOfMonth)
            case maybeDay of
                Just day -> return day
                Nothing -> do
                    putStr "Date values out of bounds" >> hFlush stdout
                    readDate
  where
    invalidSplit maybeValues = (length maybeValues /= 3) || any isNothing maybeValues

readDateAfter :: Day -> IO Day
readDateAfter date = do
    supposedDateAfter <- readDate
    if date > supposedDateAfter
        then do
            putStr "Due date should not be earlier than issue date" >> hFlush stdout
            readDateAfter date
        else return supposedDateAfter

readTin :: IO Int
readTin = do
    tin <- readPositiveIntWithError (ParseErrorMessage "  Enter valid TIN: ") (InvalidNumErrorMessage "  Enter valid TIN: ")
    if length (show tin) /= 10
        then do
            putStr "  TIN number must have length 10: " >> hFlush stdout
            readTin
        else return tin

readValidName :: InvalidNameErrorMessage -> IO String
readValidName inem = do
    name <- getLine
    if null name || length name > 50
        then do
            putStr (invalidNameErrorMessage inem) >> hFlush stdout
            readValidName inem
        else return name
