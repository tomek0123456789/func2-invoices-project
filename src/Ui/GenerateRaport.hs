module Ui.GenerateRaport (
    generateRaport,
) where

import Data.List (groupBy)
import Models.Invoice
import System.IO (hFlush, stdout)
import Ui.UiUtils (InvalidNumErrorMessage (..), ParseErrorMessage (..), readDate, readPositiveMoneyWithError, readTin, trim)

import Debug.Trace

getFilter :: IO (Invoice -> Bool)
getFilter = do
    putStrLn ""
    putStrLn "[1] Plain"
    putStrLn "[2] Issued by company (TIN)"
    putStrLn "[3] Issued before (YYYY-MM-DD)"
    putStrLn "[4] Issued after (YYYY-MM-DD)"
    putStrLn "[5] Issued after (YYYY-MM-DD)"
    putStrLn "[6] Total more than"
    putStrLn "[7] Total less than"
    putStrLn "[8] Total equal to"
    input <- trim <$> getLine
    case input of
        "1" -> return $ const True
        "2" -> do
            putStr "Enter TIN of the company: " >> hFlush stdout
            filterInput <- readTin
            return (\invoice -> (tin . sellerCompany) invoice == filterInput)
        "3" -> do
            putStr "Enter date: " >> hFlush stdout
            filterInput <- readDate
            return (\invoice -> issueDate invoice < filterInput)
        "4" -> do
            putStr "Enter date: " >> hFlush stdout
            filterInput <- readDate
            return (\invoice -> issueDate invoice > filterInput)
        "5" -> do
            putStr "Enter date: " >> hFlush stdout
            filterInput <- readDate
            return (\invoice -> issueDate invoice == filterInput)
        "6" -> do
            putStr "Enter total: " >> hFlush stdout
            filterInput <- readPositiveMoneyWithError (ParseErrorMessage "Enter valid amount: ") (InvalidNumErrorMessage "Amount must be greater than 0: ")
            return (\invoice -> amountTotal invoice > filterInput)
        "7" -> do
            putStr "Enter total: " >> hFlush stdout
            filterInput <- readPositiveMoneyWithError (ParseErrorMessage "Enter valid amount: ") (InvalidNumErrorMessage "Amount must be greater than 0: ")
            return (\invoice -> amountTotal invoice < filterInput)
        "8" -> do
            putStr "Enter total: " >> hFlush stdout
            filterInput <- readPositiveMoneyWithError (ParseErrorMessage "Enter valid amount: ") (InvalidNumErrorMessage "Amount must be greater than 0: ")
            return (\invoice -> amountTotal invoice == filterInput)
        _ -> do
            putStr "Invalid input, please enter a valid option: " >> hFlush stdout
            getFilter

generateRaport :: [Invoice] -> IO ()
generateRaport invoices = do
    putStrLn "[1] Generate raport for all invoices"
    putStrLn "[2] Generate raport for overdue invoices"
    input <- trim <$> getLine
    filterFn <- getFilter
    case input of
        "1" -> do
            generateRaportAll (filter filterFn invoices)
        "2" -> do
            generateRaportOverdue (filter filterFn invoices)
        _ -> do
            putStr "Invalid input, please enter a valid option: " >> hFlush stdout
            generateRaport invoices

generateRaportAll :: [Invoice] -> IO ()
generateRaportAll invoices = do
    trace (show (map invoiceNumber invoices)) (return ())
    let grouped = groupBy (\invoiceA invoiceB -> sellerCompany invoiceA == sellerCompany invoiceB) invoices
    mapM_ printRaport grouped

generateRaportOverdue :: [Invoice] -> IO ()
generateRaportOverdue invoices = do
    let dueInvoices = filter (\Invoice{status = status'} -> status' == Overdue) invoices
    let grouped = groupBy (\invoiceA invoiceB -> sellerCompany invoiceA == sellerCompany invoiceB) dueInvoices
    mapM_ printRaport grouped

r :: Int -> a -> [a]
r = replicate

printField :: (Show a) => Int -> a -> [Char]
printField n field = r (n - (length . show) field) ' ' ++ show field

printFieldStr :: Int -> String -> [Char]
printFieldStr n field = r (n - length field) ' ' ++ field

printRaport :: [Invoice] -> IO ()
printRaport invoices = do
    putStrLn ("\nInvoices for company: " ++ show ((companyName . sellerCompany . head) invoices) ++ ":")
    let firstString = "Invoice no.|" ++ (r 37 ' ') ++ "Buyer company|" ++ (r 7 ' ') ++ "TIN| Status|" ++ (r 10 ' ') ++ "Net|" ++ (r 4 ' ') ++ "Tax added|" ++ (r 2 ' ') ++ "Total gross| Interest|" ++ (r 5 ' ') ++ "Total|" ++ (r 2 ' ') ++ "Paid off"
    putStrLn firstString
    putStrLn (r (length firstString) '-')
    mapM_ printLinesWithInvoice invoices
    putStrLn "\n"

printLinesWithInvoice :: Invoice -> IO ()
printLinesWithInvoice
    ( Invoice
            { invoiceNumber = invoiceNumber'
            , buyerCompany = buyerCompany'
            , soldItems = soldItems'
            , amountTotal = amountTotal'
            , amountPaid = amountPaid'
            , interest =
                Interest
                    { interestInner = interestInner'
                    }
            , status = status'
            }
        ) = do
        let totalNet' = sum (map totalNet soldItems')
        let totalVat' = sum (map vatAmount soldItems')
        -- TODO change that to chain of putStr
        let firstString = printField 11 invoiceNumber' ++ "|" ++ printFieldStr 50 (companyName buyerCompany') ++ "|" ++ printField 10 (tin buyerCompany') ++ "|" ++ printField 7 status' ++ "|" ++ printField 13 totalNet' ++ "|" ++ printField 13 totalVat' ++ "|" ++ printField 13 amountTotal' ++ "|" ++ printField 9 interestInner' ++ "|" ++ printField 10 (amountTotal' + interestInner') ++ "|" ++ printField 10 amountPaid'
        putStrLn firstString
        putStrLn (r 11 ' ' ++ "|" ++ printFieldStr 50 (companyAddress buyerCompany') ++ "|" ++ (r 10 ' ') ++ "|" ++ (r 7 ' ') ++ "|" ++ (r 13 ' ') ++ "|" ++ (r 13 ' ') ++ "|" ++ (r 13 ' ') ++ "|" ++ (r 9 ' ') ++ "|" ++ (r 10 ' ') ++ "|" ++ (r 10 ' '))
        putStrLn (r (length firstString) '-')
