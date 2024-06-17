module Json.JsonFileHandler (
    saveInvoice,
    saveInvoices,
    loadInvoices,
)
where

import Data.Aeson (decodeFileStrict, encodeFile)

import Models.Invoice

saveInvoice :: Invoice -> IO ()
saveInvoice invoice = do
    -- TODO wyciagnac to z Maybe i dzialac ewentualnie inaczej na tym, jakis error gdy nie da sie pliku otworzyc
    -- funkcja n ahandlowanie gdy sie nie da otworzyc czy cos
    maybeInvoices <- decodeFileStrict "./.storage/invoices.json"
    case maybeInvoices of
        Just invoices -> do
            encodeFile "./.storage/invoices.json" (invoice : invoices)
        -- TODO handle that with `Either a b` or smth
        Nothing -> error "Couldn't decode invoices from storage file."

saveInvoices :: [Invoice] -> IO ()
saveInvoices invoices = do
    encodeFile "./.storage/invoices.json" invoices

loadInvoices :: IO [Invoice]
loadInvoices = do
    maybeInvoices <- decodeFileStrict "./.storage/invoices.json"
    -- TODO handle that somehow else, duplicated code shared with saveInvoice
    case maybeInvoices of
        Just invoices -> return (invoices :: [Invoice])
        Nothing -> error "Couldn't decode invoices from storage file."
