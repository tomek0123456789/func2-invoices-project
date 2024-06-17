module Utils.IOUtils (
    validateStorage,
) where

import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)

validateStorage :: IO ()
validateStorage = do
    -- TODO use System.FilePath for multiple platform support
    -- https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-FilePath.html
    invoicesFileExists <- doesFileExist "./.storage/invoices.json"
    unless
        invoicesFileExists
        ( do
            createDirectoryIfMissing True "./.storage"
            writeFile "./.storage/invoices.json" "[]"
        )
