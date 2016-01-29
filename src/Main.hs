{-# LANGUAGE OverloadedStrings #-}

-- Haskell imports
import qualified Snap.Core              as SC
import qualified Snap.Http.Server       as SHS

-- My imports
import qualified Routes.FileUploadRoute as FUR

site :: SC.Snap ()
site = SC.route [("fileUpload", FUR.handle)]

main :: IO ()
main = SHS.quickHttpServe site
