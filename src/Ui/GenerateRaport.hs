module Ui.GenerateRaport (
    generateRaport,
) where

import System.IO (hFlush, stdout)
import Models.Invoice
import Ui.UiUtils (trim)

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
    
    return ()

generateRaportOverdue :: [Invoice] ->  IO ()
generateRaportOverdue invoices = do
    let dueInvoices = filter (\Invoice {status=status'} -> status' == Overdue) invoices
    return ()
