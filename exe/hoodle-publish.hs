{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3 
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Main where 

import           Control.Applicative 
import           Control.Concurrent 
import           Control.Exception hiding (try)
import           Control.Lens (_1,_2,_3,_4,view,at )
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.State 
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Data
import           Data.Typeable
import           Data.Int
import qualified Data.IntMap as IM
import           Data.List 
import           Data.UUID.V4
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk (initGUI)
import           Network.HTTP.Base
import           Network.URI
import           Pdf.Toolbox.Core
import           Pdf.Toolbox.Document
import           Pdf.Toolbox.Document.Internal.Types 
import           System.Console.CmdArgs
import           System.Directory
import           System.Directory.Tree 
import           System.FilePath 
import           System.Environment
import           System.IO
import qualified System.IO.Streams as Streams
import           System.Process
-- 
import qualified Data.Hoodle.Simple as S
import           Data.Hoodle.Generic
import           Graphics.Hoodle.Render 
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Type.Background
import           Graphics.Hoodle.Render.Type.Hoodle
import           Text.Hoodle.Parse.Attoparsec 
-- 
import Debug.Trace



data UrlPath = FileUrl FilePath | HttpUrl String 
             deriving (Show,Eq)

data T = N | F | H | HS  deriving (Show,Eq)

-- | 
urlParse :: String -> Maybe UrlPath 
urlParse str = 
  if length str < 7 
    then Just (FileUrl str) 
    else 
      let p = do b <- (try (string "file://" *> return F)  
                       <|> try (string "http://" *> return H) 
                       <|> try (string "https://" *> return HS)
                       <|> (return N) )
                 rem <- manyTill anyChar ((satisfy (inClass "\r\n") *> return ()) <|> endOfInput)
                 return (b,rem) 
          r = parseOnly p (B.pack str)
      in case r of 
           Left _ -> Nothing -- Just (FileUrl str) 
           Right (b,f) -> case b of 
                            N -> Just (FileUrl f)
                            F -> Just (FileUrl (unEscapeString f))
                            H -> Just (HttpUrl ("http://" ++ f))
                            HS -> Just (HttpUrl ("https://" ++ f))
    



isFile (File _ _) = True
isFile _ = False

takeFile x | isFile x = (Just . file) x 
takeFile x | otherwise = Nothing 




-- | Get the relative url to the site root, for a given (absolute) url
toSiteRoot :: String -> String
toSiteRoot = emptyException . joinPath . map parent
           . filter relevant . splitPath . takeDirectory
  where
    parent            = const ".."
    emptyException [] = "."
    emptyException x  = x
    relevant "."      = False
    relevant "/"      = False
    relevant _        = True


data Annot = Annot { annot_rect :: (Int, Int, Int, Int) 
                   , annot_border :: (Int ,Int, Int) 
                   , annot_act :: AnnotActions
                   } 

data AnnotActions = OpenURI String | OpenApp String 

data AppState = AppState {
  stNextFree :: Int,
  stPageRefs :: [Ref],
  stRootNode :: Ref
  }

initialAppState :: AppState
initialAppState = AppState {
  stNextFree = 1,
  stPageRefs = [],
  stRootNode = error "stRootNode"
  }

nextFreeIndex :: Monad m => StateT AppState m Int
nextFreeIndex = do
  st <- get
  let index = stNextFree st
  put $ st {stNextFree = index + 1}
  return index

putPageRef :: Monad m => Ref -> StateT AppState m ()
putPageRef ref =
  modify $ \st -> st {stPageRefs = ref : stPageRefs st}

writeTrailer :: StateT AppState (PdfWriter IO) ()
writeTrailer = do
  pageRefs <- gets stPageRefs

  rootRef <- gets stRootNode
  lift $ writeObject rootRef $ ODict $ Dict [
    ("Type", OName "Pages"),
    ("Count", ONumber $ NumInt $ length pageRefs),
    ("Kids", OArray $ Array $ map ORef $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex
  let catalogRef = Ref catalogIndex 0
  lift $ writeObject catalogRef $ ODict $ Dict [("Type", OName "Catalog"), ("Pages", ORef rootRef)]

  count <- gets stNextFree
  lift $ writeXRefTable 0 (Dict [("Size", ONumber $ NumInt $ count - 1), ("Root", ORef catalogRef)])

writeObjectChildren :: Object () -> Pdf (StateT AppState (PdfWriter IO)) (Object ())
writeObjectChildren (ORef r) = do
  o <- lookupObject r
  case o of
    OStream s -> do
      ref <- writeStream s
      return $ ORef ref
    _ -> do
      let o' = mapObject (error "impossible") o
      o'' <- writeObjectChildren o'
      index <- (lift.lift) nextFreeIndex
      let ref = Ref index 0
      (lift.lift.lift) $ writeObject ref $ mapObject (error "impossible") o''
      return $ ORef ref
writeObjectChildren (ODict (Dict vals)) = do
  vals' <- forM vals $ \(key, val) -> do
    val' <- writeObjectChildren val
    return (key, val')
  return $ ODict $ Dict vals'
writeObjectChildren (OArray (Array vals)) = do
  vals' <- forM vals writeObjectChildren
  return $ OArray $ Array vals'
writeObjectChildren o = return o

-- | 
writeStream :: Stream Int64 -> Pdf (StateT AppState (PdfWriter IO)) Ref
writeStream s@(Stream dict _) = do
    len <- lookupDict "Length" dict >>= deref >>= fromObject >>= intValue
    ris <- getRIS
    Stream _ is <- rawStreamContent ris len s
    content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
    index <- (lift . lift) nextFreeIndex
    let ref = Ref index 0
    dict' <- writeObjectChildren (ODict dict) >>= fromObject
    lift . lift . lift $ writeObject ref $ OStream $ Stream dict' content
    return ref

-- |
writeAnnot :: Annot -> Pdf (StateT AppState (PdfWriter IO)) Ref
writeAnnot Annot{..} = do  
    annotIndex <- (lift.lift) nextFreeIndex
    actionIndex <- (lift.lift) nextFreeIndex
    let annotRef = Ref annotIndex 0 
        actionRef = Ref actionIndex 0 
    let annotDict = Dict [ ("Type", OName "Annot") 
                         , ("Subtype", OName "Link") 
                         , ("Rect", OArray $ Array [ ONumber (NumInt (view _1 annot_rect))
                                                   , ONumber (NumInt (view _2 annot_rect))
                                                   , ONumber (NumInt (view _3 annot_rect))
                                                   , ONumber (NumInt (view _4 annot_rect)) ] ) 
                         , ("Border", OArray $ Array [ ONumber (NumInt (view _1 annot_border))
                                                     , ONumber (NumInt (view _2 annot_border))
                                                     , ONumber (NumInt (view _3 annot_border)) ] ) 
                         , ("A", ORef actionRef) 
                         ] 
        actionDict = case annot_act of 
                       OpenURI uri -> Dict [ ("S", OName "URI" ) 
                                           , ("URI", OStr (Str (B.pack uri)))
                                           ] 
                       OpenApp str -> Dict [ ("S", OName "Launch" )
                                           , ("F", OStr (Str (B.pack str)))
                                           ] 

    lift.lift.lift $ writeObject annotRef $ ODict annotDict 
    lift.lift.lift $ writeObject actionRef $ ODict actionDict 
    return annotRef 

-- | 
writePdfPageWithAnnot :: S.Dimension -> Maybe [Annot] -> Page -> Pdf (StateT AppState (PdfWriter IO)) ()
writePdfPageWithAnnot (S.Dim w h) mannots page@(Page _ pageDict) = do
  parentRef <- lift.lift $ gets stRootNode
  pageIndex <- (lift.lift) nextFreeIndex
  let pageRef = Ref pageIndex 0
  lift.lift $ putPageRef pageRef
  contentRefs <- pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    s <- lookupObject r >>= toStream
    writeStream s
  resources <- lookupDict "Resources" pageDict >>= deref >>= writeObjectChildren
  
  case mannots of 
    Nothing -> lift.lift.lift $ writeObject pageRef $ ODict 
                 $ Dict [ ("Type", OName "Page")
                        , ("Contents", OArray $ Array $ map ORef contentRefs')
                        , ("MediaBox", OArray $ Array $ map (ONumber . NumInt) [0,0,floor w,floor h]) 
                        , ("Resources", resources)
                        , ("Parent", ORef parentRef)
                        ]
    Just anns -> do
      annrefs <- mapM writeAnnot anns
      lift.lift.lift $ writeObject pageRef $ ODict 
                 $ Dict [ ("Type", OName "Page")
                        , ("Contents", OArray $ Array $ map ORef contentRefs')
                        , ("MediaBox", OArray $ Array $ map (ONumber . NumInt) [0,0,floor w,floor h])
                        , ("Resources", resources)
                        , ("Parent", ORef parentRef)
                        , ("Annots", (OArray . Array . map ORef) annrefs) 
                        ]

-- | 
makeAnnot :: S.Dimension -> String -> (FilePath,FilePath) -> S.Link -> IO (Maybe Annot)
makeAnnot (S.Dim pw ph) urlbase (rootpath,currpath) lnk = do 
  let (x,y) = S.link_pos lnk
      S.Dim w h = S.link_dim lnk
      pwi = floor pw 
      phi = floor ph
      xi = floor x
      yi = floor y 
      wi = floor w 
      hi = floor h
      linkpath = (B.unpack . S.link_location) lnk
  case urlParse linkpath of 
    Nothing -> return Nothing 
    Just urlpath -> do 
      case urlpath of 
        HttpUrl url -> return (Just Annot { annot_rect = (xi,phi-yi,xi+wi,phi-(yi+hi))
                              , annot_border = (16,16,1) 
                              , annot_act = OpenURI url
                              })
        FileUrl path -> do 
          b <- doesFileExist linkpath 
          if b 
            then do
              fp <- canonicalizePath linkpath 
              let (dir,fn) = splitFileName fp
                  rdir = makeRelative rootpath dir 
                  (fb,ext) = splitExtension fn 
              return (Just Annot { annot_rect = (xi,phi-yi,xi+wi,phi-(yi+hi))
                                 , annot_border = (16,16,1) 
                                 , annot_act = OpenURI (urlbase </> rdir </> urlEncode fb <.> "pdf")
                                 })
            else return Nothing 

-- | 
writePdfFile :: FilePath -- ^ hoodle file path
             -> S.Dimension
             -> String -- ^ url base 
             -> (FilePath,FilePath)   -- ^ (root path, curr path)
             -> FilePath    -- ^ pdf file 
             -> [(Int,[S.Link])]
             -> StateT AppState (PdfWriter IO) ()
writePdfFile hdlfp dim urlbase (rootpath,currpath) path nlnks = do
  handle <- liftIO $ openBinaryFile path ReadMode
  res <- runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    root <- document >>= documentCatalog >>= catalogPageNode
    count <- pageNodeNKids root
    forM_ [0..count-1] $ \i -> do
      page <- pageNodePageByNum root i
      -- let dim = S.Dim 612.0 792.0 
      -- liftIO $ print dim
      mannots <- runMaybeT $ do 
                   lnks <- MaybeT . return $ lookup (i+1) nlnks
                   liftM catMaybes . mapM (liftIO . makeAnnot dim urlbase (rootpath,currpath)) $ lnks 
      hdlfp' <- liftIO $ canonicalizePath hdlfp 
      let special = if i == 0 
                    then let S.Dim w h = dim 
                         in  [ Annot { annot_rect = (0,floor h,100,floor h-100)
                                     , annot_border = (16,16,1) 
                                     , annot_act = OpenApp hdlfp'
                                     }
                             ]
                    else []  
      let mannots' = case mannots of 
                       Nothing -> Just special 
                       Just anns -> Just (anns ++ special)
       -- fmap (++ special) mannots 

      writePdfPageWithAnnot dim mannots' page
  when (isLeft res) $ error $ show res
  liftIO $ hClose handle

getLinks :: S.Page -> [S.Link]
getLinks pg = do 
  l <- view S.layers pg 
  S.ItemLink lnk <- view S.items l
  return lnk 

isHdl = ( == ".hdl") <$> takeExtension 

isPdf = ( == ".pdf") <$> takeExtension


-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m () 
sequence1_ _ []  = return () 
sequence1_ _ [a] = a 
sequence1_ i (a:as) = a >> i >> sequence1_ i as 


-- | 
renderjob :: RHoodle -> FilePath -> IO () 
renderjob h ofp = do 
  let p = maybe (error "renderjob") id $ IM.lookup 0 (view gpages h)  
  let S.Dim width height = view gdimension p  
  let rf x = cairoRenderOption (RBkgDrawPDF,DrawFull) x >> return () 
  withPDFSurface ofp width height $ \s -> renderWith s $  
    (sequence1_ showPage . map rf . IM.elems . view gpages ) h 


isUpdated :: (FilePath,FilePath) -> IO Bool 
isUpdated (ofp,nfp) = do 
  b <- doesFileExist nfp
  if not b 
    then return True
    else do 
      otime <- getModificationTime ofp
      ntime <- getModificationTime nfp 
      return (otime > ntime)
  



createPdf :: String -> FilePath -> (FilePath,FilePath) -> IO ()
createPdf urlbase rootpath (fn,ofn) = catch action (\(e :: SomeException) -> print e)
  where 
    action = do 
      putStrLn fn 
      let (odir,_) = splitFileName ofn 
      b <- doesDirectoryExist odir
      when (not b) $ system ("mkdir -p " ++ odir) >> return () 
      let (currpath,_) = splitFileName fn
      Streams.withFileAsOutput ofn $ \ostr -> do 
        bstr <- B.readFile fn 
        case parseOnly hoodle bstr of 
          Left str -> error str 
          Right hdl -> do
            let npgs = zip [1..] (view S.pages hdl)
                npglnks = map ((,) <$> fst <*> getLinks . snd) npgs  
                dim = (view S.dimension . snd . head) npgs 
            rhdl <- cnstrctRHoodle hdl 
            tempfile <- (</>) <$> getTemporaryDirectory <*> liftM show nextRandom
            renderjob rhdl tempfile
            runPdfWriter ostr $ do 
              writePdfHeader
              deleteObject (Ref 0 65535) 0 
              flip evalStateT initialAppState $ do 
                index <- nextFreeIndex 
                modify $ \st -> st { stRootNode = Ref index 0} 
                writePdfFile fn dim urlbase (rootpath,currpath) tempfile npglnks
                writeTrailer
            removeFile tempfile 


--------------------------------------------------------------------
--          main program                                          --
--------------------------------------------------------------------

data HoodlePublish = Publish { urlbase :: String 
                             , rootpath :: FilePath
                             , buildpath :: FilePath
                             }
                     deriving (Show,Data,Typeable)


publish :: HoodlePublish 
publish = Publish { urlbase = def &= typ "URLBASE" &= argPos 0 
                  , rootpath = def &= typ "ORIGNALFILEDIR" &= argPos 1 
                  , buildpath = def &= typ "TARGETFILEDIR" &= argPos 2 
                  }

mode :: HoodlePublish 
mode = modes [publish] 

-- | 
main :: IO ()
main = do
  initGUI 
  params <- cmdArgs mode 
  (r :/ r') <- build (rootpath params)
  let files = catMaybes . map takeFile . flattenDir $ r' 
      hdlfiles = filter isHdl files 
      pairs = map ((,) <$> id
                   <*> (buildpath params </>) . flip replaceExtension "pdf" . makeRelative (rootpath params)) 
                  hdlfiles 
      swappedpairs = map (\(x,y)->(y,x)) pairs 
  (b :/ b') <- build (buildpath params)
  let files2 = catMaybes . map takeFile . flattenDir $ b' 
      pdffiles = filter isPdf files2
      willbeerased = filter (\x -> isNothing (lookup x swappedpairs )) pdffiles 
      
  mapM_ removeFile willbeerased 
      
  updatedpairs <- filterM isUpdated pairs 
  mapM_ (createPdf (urlbase params) (rootpath params)) updatedpairs
