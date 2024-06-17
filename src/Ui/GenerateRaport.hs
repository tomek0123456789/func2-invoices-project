module Ui.GenerateRaport (
    generateRaport,
) where

import System.IO (hFlush, stdout)
import Models.Invoice
import Data.List (groupBy)
import Ui.UiUtils (trim)

import Debug.Trace

generateRaport :: [Invoice] ->  IO ()
generateRaport invoices = do 
    putStrLn "[1] Generate raport for all invoices"
    putStrLn "[2] Generate raport for overdue invoices"
    input <- trim <$> getLine
    case input of 
        "1" -> do
            generateRaportAll invoices
            return ()
        "2" -> do 
            generateRaportOverdue invoices
            return ()
        _ -> do
            putStr "Invalid input, please enter a valid option: " >> hFlush stdout
            generateRaport invoices

generateRaportAll :: [Invoice] ->  IO ()
generateRaportAll invoices = do
    let grouped = groupBy (\invoiceA invoiceB -> sellerCompany invoiceA == sellerCompany invoiceB) invoices 
    putStrLn ("\nDue invoices for company: " ++ show ((companyName . sellerCompany . head) invoices) ++ ":")
    mapM_ printRaport grouped 

generateRaportOverdue :: [Invoice] ->  IO ()
generateRaportOverdue invoices = do
    let dueInvoices = filter (\Invoice {status=status'} -> status' == Overdue) invoices
    let grouped = groupBy (\invoiceA invoiceB -> sellerCompany invoiceA == sellerCompany invoiceB) dueInvoices
    putStrLn ("\nOverdue invoices for company: " ++ show ((companyName . sellerCompany . head) invoices) ++ ":")
    mapM_ printRaport grouped 

r = replicate 

printField n field = r (n - (length . show) field) ' ' ++ show field

printRaport :: [Invoice] -> IO ()
printRaport invoices = do
    let firstString = "Invoice no.|" ++ (r 37 ' ') ++"Buyer company|" ++  (r 7 ' ') ++ "TIN|" ++ (r 10 ' ') ++ "Net|" ++  (r 4 ' ') ++ "Tax added|" ++ (r 2 ' ') ++ "Total gross| Interest|" ++ (r 5 ' ') ++ "Total|" ++ (r 2 ' ') ++ "Paid off" 
    putStrLn firstString
    putStrLn (r (length firstString) '-')
    mapM_ printLinesWithInvoice invoices
    putStrLn "\n"

printLinesWithInvoice :: Invoice -> IO ()
printLinesWithInvoice ( Invoice
            { invoiceNumber = invoiceNumber'
            , buyerCompany = buyerCompany'
            , soldItems = soldItems'
            , amountTotal = amountTotal'
            , amountPaid = amountPaid'
            , interest =
                Interest
                    { interestInner = interestInner'
                    }
            -- , issueDate = issueDate'
            -- , dueDate = dueDate'
            -- , status = status'
            }
        ) = do
    let totalNet' = sum (map totalNet soldItems')
    let totalVat' = sum (map vatAmount soldItems')
    let firstString = printField 11 invoiceNumber' ++ "|" ++ printField 50 (companyName buyerCompany') ++ "|" ++ printField 10 (tin buyerCompany') ++ "|" ++ printField 13 totalNet' ++ "|" ++ printField 13 totalVat' ++ "|" ++ printField 13 amountTotal' ++ "|" ++ printField 9 interestInner' ++ "|" ++ printField 10 (amountTotal' + interestInner') ++ "|" ++ printField 10 amountPaid'
    putStrLn firstString
    putStrLn (r 11 ' ' ++ "|" ++ printField 50 (companyAddress buyerCompany') ++ "|" ++ (r 10 ' ') ++ "|" ++ (r 13 ' ')++ "|" ++ (r 13 ' ')++ "|" ++ (r 13 ' ') ++ "|" ++ (r 9 ' ')++ "|" ++ (r 10 ' ') ++ "|" ++ (r 10 ' ')) 
    putStrLn (r (length firstString) '-')

