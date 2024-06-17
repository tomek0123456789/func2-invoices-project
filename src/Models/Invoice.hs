{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Invoice (
    InvoiceStatus (..),
    Company (..),
    InvoiceItem (..),
    Interest (..),
    Invoice (..),
    Money,
    validateInvoices,
) where

-- import Data.Time.Calendar.OrdinalDate (Day, showOrdinalDate)

import Data.Aeson
import Data.Fixed
import Data.Time.Calendar (Day, diffDays)
import GHC.Generics

import Utils.Utils (getCurrentDate)

-- import Debug.Trace

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
    , perItemNet :: Money -- TODO some money type?
    , totalNet :: Money -- TODO some money type?
    , vat :: Money -- TODO some money type?
    , vatAmount :: Money -- TODO some money type?
    , total :: Money -- TODO some money type?
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
    , amountTotal :: Money -- TODO some money type?
    , amountPaid :: Money -- TODO some money type?
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
        -- trace ("ID: " ++ show invoiceNumber' ++ ", Current date: " ++ show currentDate ++ ", nextInterestDate': " ++ show nextInterestDate' ++ ", equal? " ++ show (currentDate == nextInterestDate')) (return ())
        -- trace (show $ ((amountTotal' * (0.1 :: Money)) / 365) * fromIntegral dueDateDifference) (return ())
        -- trace ("dueDateDifference: " ++ show dueDateDifference) (return ())
        -- nextInterestDate' <- getCurrentDateSuccessor
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
                , status = if dueDateDifference > 0 && nextInterestDate' <= currentDate then Overdue else status'
                }
