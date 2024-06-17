module Ui.PayOffInvoice (
    payOffInvoice,
) where

import Data.List (find)
import Data.Maybe (fromJust)
import Models.Invoice
import System.IO (hFlush, stdout)
import Ui.UiUtils (InvalidNumErrorMessage (..), ParseErrorMessage (..), readPositiveMoneyWithError, readValidatedIntWithError)

payOffInvoice :: [Invoice] -> IO [Invoice]
payOffInvoice invoices = do
    putStr "Enter invoice number you want to pay off: " >> hFlush stdout
    invoiceIndex <- readValidatedIntWithError (\x -> x > 0 && x <= length invoices) (ParseErrorMessage "Enter valid number: ") (InvalidNumErrorMessage $ "Invoice number must be between 1 and " ++ show (length invoices) ++ ": ")
    let soughtInvoice = fromJust $ find (\Invoice{invoiceNumber = invoiceNumber'} -> invoiceNumber' == invoiceIndex) invoices

    if amountPaid soughtInvoice == amountTotal soughtInvoice
        then do
            putStrLn ("Invoice no. " ++ show invoiceIndex ++ " has already been paid off.")
            return invoices
        else do
            putStrLn ("Total amount to pay off: " ++ show (amountTotal soughtInvoice) ++ ". Paid off: " ++ show (amountPaid soughtInvoice) ++ ". Interest: " ++ show (interestInner $ interest soughtInvoice) ++ ".")
            putStr ("How much does " ++ show (companyName $ buyerCompany soughtInvoice) ++ " wants to pay off? ") >> hFlush stdout
            moneyToPayOff <- readPositiveMoneyWithError (ParseErrorMessage "Enter valid amount: ") (InvalidNumErrorMessage "Amount must be greater than 0: ")

            updatedInvoice <- updateInvoicePayment moneyToPayOff soughtInvoice
            return $ updatedInvoice : (filter (\Invoice{invoiceNumber = invoiceNumber'} -> invoiceNumber' /= invoiceIndex) invoices)

updateInvoicePayment :: Money -> Invoice -> IO Invoice
-- handles interest, then if something is left handles amountPaid
updateInvoicePayment
    payOffAmount
    ( Invoice
            { invoiceNumber = invoiceNumber'
            , sellerCompany = sellerCompany'
            , buyerCompany = buyerCompany'
            , soldItems = soldItems'
            , amountTotal = amountTotal'
            , amountPaid = amountPaid'
            , interest =
                Interest
                    { interestInner = interestInner'
                    , nextInterestDate = nextInterestDate'
                    }
            , issueDate = issueDate'
            , dueDate = dueDate'
            , status = status'
            }
        ) = do
        if interestInner' > payOffAmount
            then
                return
                    ( Invoice
                        { invoiceNumber = invoiceNumber'
                        , sellerCompany = sellerCompany'
                        , buyerCompany = buyerCompany'
                        , soldItems = soldItems'
                        , amountTotal = amountTotal'
                        , amountPaid = amountPaid'
                        , interest =
                            Interest
                                { interestInner = interestInner' - payOffAmount
                                , nextInterestDate = nextInterestDate'
                                }
                        , issueDate = issueDate'
                        , dueDate = dueDate'
                        , status = status'
                        }
                    )
            else
                updateInvoiceAmountTotal
                    (payOffAmount - interestInner')
                    ( Invoice
                        { invoiceNumber = invoiceNumber'
                        , sellerCompany = sellerCompany'
                        , buyerCompany = buyerCompany'
                        , soldItems = soldItems'
                        , amountTotal = amountTotal'
                        , amountPaid = amountPaid'
                        , interest =
                            Interest
                                { interestInner = 0
                                , nextInterestDate = nextInterestDate'
                                }
                        , issueDate = issueDate'
                        , dueDate = dueDate'
                        , status = status'
                        }
                    )

updateInvoiceAmountTotal :: Money -> Invoice -> IO Invoice
updateInvoiceAmountTotal
    payOffAmount
    ( Invoice
            { invoiceNumber = invoiceNumber'
            , sellerCompany = sellerCompany'
            , buyerCompany = buyerCompany'
            , soldItems = soldItems'
            , amountTotal = amountTotal'
            , -- , amountPaid = amountPaid'
            interest =
                Interest
                    { interestInner = interestInner'
                    , nextInterestDate = nextInterestDate'
                    }
            , issueDate = issueDate'
            , dueDate = dueDate'
            , status = status'
            }
        ) = do
        if payOffAmount < amountTotal'
            then
                return
                    ( Invoice
                        { invoiceNumber = invoiceNumber'
                        , sellerCompany = sellerCompany'
                        , buyerCompany = buyerCompany'
                        , soldItems = soldItems'
                        , amountTotal = amountTotal'
                        , amountPaid = payOffAmount
                        , interest =
                            Interest
                                { interestInner = interestInner'
                                , nextInterestDate = nextInterestDate'
                                }
                        , issueDate = issueDate'
                        , dueDate = dueDate'
                        , status = status'
                        }
                    )
            else -- else if payOffAmount == amountTotal' then

                return
                    ( Invoice
                        { invoiceNumber = invoiceNumber'
                        , sellerCompany = sellerCompany'
                        , buyerCompany = buyerCompany'
                        , soldItems = soldItems'
                        , amountTotal = amountTotal'
                        , amountPaid = amountTotal'
                        , interest =
                            Interest
                                { interestInner = interestInner'
                                , nextInterestDate = nextInterestDate'
                                }
                        , issueDate = issueDate'
                        , dueDate = dueDate'
                        , status = Paid
                        }
                    )

-- else
--     return (Invoice
--             { invoiceNumber = invoiceNumber'
--             , sellerCompany = sellerCompany'
--             , buyerCompany = buyerCompany'
--             , soldItems = soldItems'
--             , amountTotal = amountTotal'
--             , amountPaid = payOffAmount
--             , interest = Interest {
--                 interestInner = interestInner',
--                 nextInterestDate = nextInterestDate'
--             }
--             , issueDate = issueDate'
--             , dueDate = dueDate'
--             , status = Overpaid
--             })
