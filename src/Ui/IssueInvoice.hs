module Ui.IssueInvoice (
    getCompanyData,
    getInvoiceItem,
    getInvoiceItems,
    getInvoice,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Models.Invoice (Company (..), Interest (..), Invoice (..), InvoiceItem (..), InvoiceStatus (..))
import System.IO (hFlush, stdout)
import Ui.UiUtils (InvalidNameErrorMessage (..), InvalidNumErrorMessage (..), ParseErrorMessage (..), readDate, readDateAfter, readPositiveIntWithError, readPositiveMoneyWithError, readTin, readValidName, readValidatedMoneyWithError)

getCompanyData :: IO Company
getCompanyData = do
    putStr "  Company name: " >> hFlush stdout
    inputCompanyName <- readValidName (InvalidNameErrorMessage "  Enter a valid company name: ")
    putStr "  Company address: " >> hFlush stdout
    inputAddress <- readValidName (InvalidNameErrorMessage "  Enter a valid company address: ")
    putStr "  TIN: " >> hFlush stdout
    inputTin <- readTin
    return $ Company{companyName = inputCompanyName, companyAddress = inputAddress, tin = inputTin}

getInvoiceItem :: Int -> IO InvoiceItem
getInvoiceItem inputItemNumber = do
    putStrLn $ "Item no. " ++ show inputItemNumber
    putStr "  Name: " >> hFlush stdout
    inputItemName <- readValidName (InvalidNameErrorMessage "  Enter a valid item name: ")
    putStr "  Quantity: " >> hFlush stdout
    inputQuantity <- readPositiveIntWithError (ParseErrorMessage "  Enter valid item quantity: ") (InvalidNumErrorMessage "  Item quantity should be greater than 0: ")
    putStr "  Price per unit (net): " >> hFlush stdout
    inputPerItemNet <- readPositiveMoneyWithError (ParseErrorMessage "  Enter a valid price: ") (InvalidNumErrorMessage "  Price per item should be greater than 0: ")
    putStr "  VAT (percent): " >> hFlush stdout
    inputVat <- readValidatedMoneyWithError (>= 0) (ParseErrorMessage "  Enter valid VAT: ") (InvalidNumErrorMessage "  VAT should not be negative: ")
    let calculatedTotalNet = fromIntegral inputQuantity * inputPerItemNet
    let calculatedVatAmount = (inputVat / 100) * calculatedTotalNet
    return $
        InvoiceItem
            { itemNumber = inputItemNumber
            , itemName = inputItemName
            , quantity = inputQuantity
            , perItemNet = inputPerItemNet
            , totalNet = calculatedTotalNet
            , vat = calculatedTotalNet
            , vatAmount = calculatedVatAmount
            , total = calculatedTotalNet + calculatedVatAmount
            }

getInvoiceItems :: Int -> StateT Int IO [InvoiceItem]
getInvoiceItems 0 = return []
getInvoiceItems n = do
    inputItemNumber <- get
    invoiceItem <- lift $ getInvoiceItem inputItemNumber
    modify (+ 1)
    otherItems <- getInvoiceItems $ n - 1
    return $ invoiceItem : otherItems

getInvoice :: Int -> IO Invoice
getInvoice inputInvoiceNumber = do
    putStrLn "Seller company data:"
    inputSellerCompany <- getCompanyData
    putStrLn "Buyer company data:"
    inputBuyerCompany <- getCompanyData
    putStr "Number of items: " >> hFlush stdout
    itemCount <- readPositiveIntWithError (ParseErrorMessage "Enter valid number: ") (InvalidNumErrorMessage "Number must be greater than 0: ")
    inputItems <- evalStateT (getInvoiceItems itemCount) 1
    putStr "Invoice issue date (YYYY-MM-DD): " >> hFlush stdout
    inputIssueDate <- readDate
    putStr "Invoice due date (YYYY-MM-DD): " >> hFlush stdout
    inputDueDate <- readDateAfter inputIssueDate

    let invoice =
            Invoice
                { invoiceNumber = inputInvoiceNumber
                , sellerCompany = inputSellerCompany
                , buyerCompany = inputBuyerCompany
                , soldItems = inputItems
                , amountTotal = sum (map total inputItems)
                , amountPaid = 0.0
                , interest =
                    Interest
                        { interestInner = 0.0
                        , nextInterestDate = succ inputDueDate
                        }
                , issueDate = inputIssueDate
                , dueDate = inputDueDate
                , status = Due
                }
    return invoice
