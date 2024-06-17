module Ui.UiHandler (
    mainMenuLoopWithWelcome,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Json.JsonFileHandler (loadInvoices, saveInvoice, saveInvoices)
import Models.Invoice
import System.IO (hFlush, stdout)
import Ui.GenerateRaport (generateRaport)
import Ui.IssueInvoice (getInvoice)
import Ui.PayOffInvoice (payOffInvoice)
import Ui.UiUtils (trim)
import Utils.IOUtils (validateStorage)

printMainMenu :: IO ()
printMainMenu = do
    putStrLn "[1] Invoices raport"
    putStrLn "[2] Issue an invoice"
    putStrLn "[3] Pay off an invoice"
    putStrLn "[4] Exit"

mainMenuLoopWithWelcome :: IO ()
mainMenuLoopWithWelcome = do
    putStrLn "===================================="
    putStrLn "INVOICE SYSTEM HANDLER V0.0.1 SUPERB"
    putStrLn "===================================="
    validateStorage
    mainMenuLoop

mainMenuLoop :: IO ()
mainMenuLoop = do
    printMainMenu
    handleMainMenuInput

handleMainMenuInput :: IO ()
handleMainMenuInput = do
    action <- trim <$> getLine
    case action of
        "1" -> do
            invoices <- loadInvoices >>= validateInvoices
            runReaderT handleGenerateRaport invoices
            mainMenuLoop
        "2" -> do
            invoices <- loadInvoices
            runReaderT handleIssueInvoice (length invoices)
            mainMenuLoop
        "3" -> do
            invoices <- loadInvoices >>= validateInvoices
            runReaderT handlePayOffInvoice invoices
            mainMenuLoop
        "4" -> do
            putStrLn "Goodbye!"
            return ()
        _ -> do
            putStr "Invalid input, please enter a valid option: " >> hFlush stdout
            handleMainMenuInput

handleGenerateRaport :: ReaderT [Invoice] IO ()
handleGenerateRaport = do
    invoices <- ask
    lift $ generateRaport invoices
    return ()

handleIssueInvoice :: ReaderT Int IO ()
handleIssueInvoice = do
    envInvoiceNumber <- ask
    invoice <- lift $ getInvoice (envInvoiceNumber + 1)
    lift $ do
        saveInvoice invoice
        putStrLn "\nSuccessfully added an invoice to the system\n"
    return ()

handlePayOffInvoice :: ReaderT [Invoice] IO ()
handlePayOffInvoice = do
    invoices <- ask
    lift $ do
        handledInvoices <- payOffInvoice invoices
        saveInvoices handledInvoices
    return ()
