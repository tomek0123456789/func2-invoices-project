module Utils.Utils (
    getCurrentDate,
    getCurrentDateSuccessor,
)
where

import Data.Time.Calendar
import Data.Time.Clock

-- import Data.List.Split (splitOn)

getCurrentDate :: IO Day
getCurrentDate = getCurrentTime >>= return . utctDay

getCurrentDateSuccessor :: IO Day
getCurrentDateSuccessor = getCurrentDate >>= return . succ

-- TODO type that better
-- dateStringToTriple :: String -> IO (Int, Int, Int)
-- dateStringToTriple dateString = do
--     let splitElems = splitOn "-" dateString
--     return (read (splitElems !! 0) :: Int, read (splitElems !! 1) :: Int, read (splitElems !! 2) :: Int)

-- test = fromGregorianValid 2099 13 12

-- Get the current UTC time
-- currentTime <- getCurrentTime
-- putStrLn $ "Current UTC Time: " ++ show currentTime

-- -- Parse a date string
-- let dateStr = "2022-12-25"
-- let parsedDate = readTime defaultTimeLocale "%Y-%m-%d" dateStr :: Day
-- putStrLn $ "Parsed Date: " ++ show parsedDate

-- -- Add days to a date
-- let futureDate = addDays 100 parsedDate
-- putStrLn $ "Future Date: " ++ show futureDate

-- jsonString :: BL.ByteString
-- jsonString = BLU.fromString "{\"invoiceNumber\":1,\"sellerCompany\":{\"companyName\":\"UJ_WMII\",\"companyAddress\":\"Krakow Lojasiewicza 6/3\",\"tin\":1234567},\"buyerCompany\":{\"companyName\":\"misktom\",\"companyAddress\":\"Krakow Zablocie 26/100\",\"tin\":2137},\"soldItems\":[{\"no\":1,\"itemName\":\"depresja\",\"quantity\":1,\"perItemNet\":90,\"totalNet\":90,\"vat\":0,\"vatAmount\":0,\"total\":90}],\"amountTotal\":90,\"amountPaid\":90,\"interest\":0,\"issueDate\":\"2020-09-01\",\"dueDate\":\"2020-10-01\",\"status\":\"Paid\"}"

-- test :: Maybe [Invoice]
-- test = decode jsonString

-- iotest :: IO [Invoice]
-- iotest = do
--     case test of
--         Just x -> return x
--         Nothing -> return []

-- jsonString :: BL.ByteString
-- jsonString = "[{\"invoiceNumber\":1,\"sellerCompany\":{\"companyName\":\"UJ_WMII\",\"companyAddress\":\"Krakow Lojasiewicza 6/3\",\"tin\":1234567},\"buyerCompany\":{\"companyName\":\"misktom\",\"companyAddress\":\"Krakow Zablocie 26/100\",\"tin\":2137},\"soldItems\":[{\"no\":1,\"itemName\":\"depresja\",\"quantity\":1,\"perItemNet\":90,\"totalNet\":90,\"vat\":0,\"vatAmount\":0,\"total\":90}],\"amountTotal\":90,\"amountPaid\":90,\"interest\":0,\"issueDate\":\"2020-09-01\",\"dueDate\":\"2020-10-01\",\"status\":\"Paid\"}]"

-- test :: Maybe [Invoice]
-- test = decode jsonString

-- test' = do
--     let x = test
--     case x of
--         Just asdf -> saveInvoice $ head asdf
--         _ -> return ()
