{- |
File        : FileUploadRoute.hs
Module      : Routes.FileUploadRoute
Description : Route to handle file uploads
License     : MIT

Maintainer  : Umayah Abdennabi
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# RULES "ConduitM: liftIO x >>= f" forall m (f :: (Monad n, MonadIO n) => CounduitM i o n r).
    liftIO m >>= f = ConduitM (PipeM (liftM (unConduitM . f) (liftIO m)))
 #-}

module Routes.FileUploadRoute (handle) where

  -- Haskell imports
  import qualified Control.Monad.IO.Class as CMIC
  import qualified Data.ByteString.Char8  as DBC 
  import qualified Data.List              as DL
  import qualified Snap.Core              as SC  
  import qualified Snap.Util.FileUploads  as SUF
  import qualified System.Directory       as SD
  import qualified System.Random          as SR
  import qualified Text.Regex             as TR

  dirPrefix = "../../"

  ext2dir :: String -> String
  ext2dir fileType = case fileType of 
      "video/mp4"       -> "mp4"
      "video/quicktime" -> "mov"
      "image/png"       -> "png"
      "image/jpg"       -> "jpg"
      "text/csv"        -> "csv"
      "text/plain"      -> "txt"
      _                 -> "other"

  randomStr :: SC.MonadSnap m => Int -> m String
  randomStr size = do
    gen <- CMIC.liftIO $ SR.newStdGen 
    return . take size . SR.randomRs ('a', 'z') $ gen 


  splitOnExt :: String -> [String]
  splitOnExt file = 
    [filename, ext]
    where 
      split    = TR.splitRegex (TR.mkRegex "(\\.)") $ file 
      filename = concat . DL.intersperse "." . init $ split
      ext      = last split 
      

  createFile :: SC.MonadSnap m => String -> String -> String -> m ()
  createFile tempDir fileName fileType = do 
    let newName' = splitOnExt fileName
    token <- randomStr 10
    let newName = (head newName') ++ "-" ++ token ++ "." ++ (last newName')
    let newDir = dirPrefix ++ (ext2dir fileType) ++ "/" ++ newName
    CMIC.liftIO $ SD.copyFile tempDir newDir
         

  maxMb = 1024
  mb = 2 ^ (20 :: Int)

  filePolicy :: SUF.UploadPolicy
  filePolicy = SUF.setMaximumFormInputSize (maxMb * mb) SUF.defaultUploadPolicy
  
  perPartPolicy :: SUF.PartInfo -> SUF.PartUploadPolicy
  perPartPolicy _ = SUF.allowWithMaximumSize (maxMb * mb)

  modifyResponse :: SC.MonadSnap m => Int -> String -> m ()
  modifyResponse err message = 
    SC.modifyResponse . SC.setResponseStatus err $ (DBC.pack message)

  sendError :: SC.MonadSnap m => Int -> String -> String -> m ()
  sendError err message writeMsg = do
    _ <- modifyResponse err message
    SC.writeBS . DBC.pack $ writeMsg
   

  fileUploadLogic :: SC.MonadSnap m => SUF.PartInfo -> FilePath -> m ()
  fileUploadLogic partInfo filePath = do
    case (fileName, fileType) of
      ("", "")  -> sendError 400 "Missing file name and type in request" "ERROR"
      ("", _)   -> sendError 400 "Missing file name in request" "ERROR"
      (_, "")   -> sendError 400 "Missing file type in request" "ERROR"
      otherwise -> createFile filePath fileName fileType
    where 
      fileName = case (SUF.partFileName partInfo) of 
          Just x  -> DBC.unpack x 
          Nothing -> "" 
      fileType = DBC.unpack . SUF.partContentType $ partInfo

  uploadHandler :: SC.MonadSnap m
                => [(SUF.PartInfo, Either SUF.PolicyViolationException FilePath)] 
                -> m ()
  uploadHandler xs = case allValid of
      True  -> mapM_ handleOne xs 
      False -> modifyResponse 400 "Unable to upload file"
    where
      wanted (_, Left _)  = False
      wanted (_, Right _) = True
      allValid = length xs == (length . filter wanted $ xs)
      handleOne (partInfo, Right filePath) = fileUploadLogic partInfo filePath
    
 
  handleFiles :: SC.MonadSnap m => FilePath -> m ()
  handleFiles tempDir = do 
    SUF.handleFileUploads tempDir filePolicy perPartPolicy uploadHandler
    
  handle :: SC.MonadSnap m => m ()
  handle = do
    handleFiles $ dirPrefix ++ "tmp"


