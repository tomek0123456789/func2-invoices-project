{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Invoice (
    InvoiceStatus (..),
    Company (..),
    InvoiceItem (..),
    Interest (..),
    Invoice (..),
    Money,
    validateInvoices,
) where


import Data.Aeson
import Data.Fixed
import Data.Time.Calendar (Day, diffDays)
import GHC.Generics
import Utils.Utils (getCurrentDate)


type Money = Centi

data InvoiceStatus = Due | Paid | Overdue deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Company = Company
    { companyName :: String
    , companyAddress :: String
    , tin :: Int -- Taxpayer Identification Number (TIN)
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data InvoiceItem = InvoiceItem
    { itemNumber :: Int
    , itemName :: String
    , quantity :: Int
    , perItemNet :: Money
    , totalNet :: Money
    , vat :: Money
    , vatAmount :: Money 
    , total :: Money
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data Interest = Interest
    { interestInner :: Money
    , nextInterestDate :: Day
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data Invoice = Invoice
    { invoiceNumber :: Int -- TODO conside a type with nonnegative values
    , sellerCompany :: Company
    , buyerCompany :: Company
    , soldItems :: [InvoiceItem]
    , amountTotal :: Money
    , amountPaid :: Money
    , interest :: Interest
    , issueDate :: Day
    , dueDate :: Day
    , status :: InvoiceStatus
    }
    deriving (Show, Generic, ToJSON, FromJSON)

validateInvoices :: [Invoice] -> IO [Invoice]
validateInvoices = mapM validateInvoice

validateInvoice :: Invoice -> IO Invoice
validateInvoice
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
        currentDate <- getCurrentDate
        let dueDateDifference = diffDays currentDate dueDate'
        return $
            Invoice
                { invoiceNumber = invoiceNumber'
                , sellerCompany = sellerCompany'
                , buyerCompany = buyerCompany'
                , soldItems = soldItems'
                , amountTotal = amountTotal'
                , amountPaid = amountPaid'
                , -- interest 10% per year
                  interest =
                    Interest
                        { interestInner = if dueDateDifference > 0 && nextInterestDate' <= currentDate then ((amountTotal' * (0.1 :: Money)) / 365) * fromIntegral dueDateDifference else interestInner'
                        , nextInterestDate = if dueDateDifference > 0 && nextInterestDate' <= currentDate then succ currentDate else nextInterestDate'
                        }
                , issueDate = issueDate'
                , dueDate = dueDate'
                , status = if dueDateDifference > 0 && nextInterestDate' <= currentDate && status' /= Paid then Overdue else status'
                }
