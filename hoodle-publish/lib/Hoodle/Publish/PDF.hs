{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Publish.PDF
-- Copyright   : (c) 2013,2014 Ian-Woo Kim
--
-- License     : GPL-3 
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Publish.PDF where
 
import           Control.Applicative 
import           Control.Exception (SomeException(..),catch)
import           Control.Lens (_1,_2,_3,_4,view)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.State 
import           Data.Attoparsec.ByteString.Char8 
                   (parseOnly,anyChar,satisfy,inClass,endOfInput,try,string,manyTill)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import           Data.UUID (fromString, UUID(..))
import           Data.UUID.V4
import           Graphics.Rendering.Cairo
import           Network.HTTP.Base
import           Network.URI (unEscapeString)
import           Pdf.Toolbox.Core
import           Pdf.Toolbox.Document
import           Pdf.Toolbox.Document.Internal.Types 
import           System.Directory
import           System.Directory.Tree (DirTree(..))
import           System.FilePath 
import           System.IO
import qualified System.IO.Streams as Streams
import           System.Process
-- 
import qualified Data.Hoodle.Simple as S
import           Graphics.Hoodle.Render 
import           Text.Hoodle.Parse.Attoparsec (hoodle)


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
                   remain <- manyTill anyChar ((satisfy (inClass "\r\n") *> return ()) <|> endOfInput)
                   return (b,remain) 
            r = parseOnly p (B.pack str)
        in case r of 
             Left _ -> Nothing 
             Right (b,f) -> case b of 
                              N -> Just (FileUrl f)
                              F -> Just (FileUrl (unEscapeString f))
                              H -> Just (HttpUrl ("http://" ++ f))
                              HS -> Just (HttpUrl ("https://" ++ f))

isFile :: DirTree a -> Bool    
isFile (File _ _) = True
isFile _ = False

takeFile :: DirTree a -> Maybe a
takeFile x | isFile x = (Just . file) x 
takeFile _ | otherwise = Nothing 

data Annot = Annot { annot_rect :: (Int, Int, Int, Int) 
                   , annot_border :: (Int ,Int, Int) 
                   , annot_act :: AnnotActions
                   } 

data AnnotActions = OpenURI String | OpenApp String 

data AppState = AppState { stNextFree :: Int
                         , stPageRefs :: [Ref]
                         , stRootNode :: Ref
                         }

initialAppState :: AppState
initialAppState = AppState { stNextFree = 1
                           , stPageRefs = []
                           , stRootNode = error "stRootNode"
                           }

nextFreeIndex :: Monad m => StateT AppState m Int
nextFreeIndex = do
    st <- get
    let index = stNextFree st
    put $ st {stNextFree = index + 1}
    return index

putPageRef :: Monad m => Ref -> StateT AppState m ()
putPageRef ref = modify $ \st -> st {stPageRefs = ref : stPageRefs st}

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
    n <- gets stNextFree
    lift $ writeXRefTable 0 (Dict [("Size", (ONumber . NumInt) (n-1)), ("Root", ORef catalogRef)])

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
writePdfPageWithAnnot (S.Dim w h) mannots pg@(Page _ pageDict) = do
    parentRef <- lift.lift $ gets stRootNode
    pageIndex <- (lift.lift) nextFreeIndex
    let pageRef = Ref pageIndex 0
    lift.lift $ putPageRef pageRef
    contentRefs <- pageContents pg
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
makeAnnot (S.Dim _pw ph) urlbase (rootpath,_currpath) lnk = do 
    let (x,y) = S.link_pos lnk
        S.Dim w h = S.link_dim lnk
        -- pwi = floor pw 
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
          FileUrl _path -> do 
            b <- doesFileExist linkpath 
            if b 
              then do
                fp <- canonicalizePath linkpath 
                let (dir,fn) = splitFileName fp
                    rdir = makeRelative rootpath dir 
                    (fb,_ext) = splitExtension fn 
                return (Just Annot { annot_rect = (xi,phi-yi,xi+wi,phi-(yi+hi))
                                   , annot_border = (16,16,1) 
                                   , annot_act = OpenURI (urlbase </> rdir </> urlEncode fb <.> "pdf")
                                   })
              else return Nothing 

-- | 
writePdfFile :: FilePath -- ^ hoodle file path
             -> S.Dimension
             -> (String,String) -- ^ (url base, special url base (for executing an app))
             -> (FilePath,FilePath)   -- ^ (root path, curr path)
             -> FilePath    -- ^ pdf file 
             -> [(Int,[S.Link])]
             -> Maybe UUID
             -> StateT AppState (PdfWriter IO) ()
writePdfFile hdlfp dim (urlbase,specialurlbase) (rootpath,currpath) path nlnks muuid = do
    handle <- liftIO $ openBinaryFile path ReadMode
    res <- runPdfWithHandle handle knownFilters $ do
      encrypted <- isEncrypted
      when encrypted $ setUserPassword defaultUserPassword >> return ()
      root <- document >>= documentCatalog >>= catalogPageNode
      count <- pageNodeNKids root
      forM_ [0..count-1] $ \i -> do
        page <- pageNodePageByNum root i
        mannots <- runMaybeT $ do 
                     lnks <- MaybeT . return $ lookup (i+1) nlnks
                     liftM catMaybes . mapM (liftIO . makeAnnot dim urlbase (rootpath,currpath)) $ lnks 
        hdlfp' <- liftIO $ canonicalizePath hdlfp 
        let (hdldir,hdlfn) = splitFileName hdlfp' 
            (hdlfb,_ext) = splitExtension hdlfn
        let special = if i == 0 
                      then let S.Dim _w h = dim 
                           in  [ Annot { annot_rect = (0,floor h,100,floor h-100)
                                       , annot_border = (16,16,1) 
                                       , annot_act = specialURIFunction specialurlbase muuid -- (hdldir,hdlfb) 
                                       }
                               ]
                      else []  
        let mannots' = case mannots of 
                         Nothing -> Just special 
                         Just anns -> Just (anns ++ special)
        writePdfPageWithAnnot dim mannots' page
    when (isLeft res) $ error $ show res
    liftIO $ hClose handle

specialURIFunction :: FilePath -> Maybe UUID {- (FilePath,FilePath) -} -> AnnotActions
specialURIFunction baseurl muuid {- (hdldir,hdlfb) -} = 
    case muuid of
      Nothing -> error "muuid = Nothing?" -- OpenURI baseurl
      Just uuid -> OpenURI (baseurl  </>  urlEncode (show uuid))

getLinks :: S.Page -> [S.Link]
getLinks pg = do 
    l <- view S.layers pg 
    S.ItemLink lnk <- view S.items l
    return lnk 

isHdl :: FilePath -> Bool
isHdl = ( == ".hdl") <$> takeExtension 

isPdf :: FilePath -> Bool
isPdf = ( == ".pdf") <$> takeExtension


-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m () 
sequence1_ _ []  = return () 
sequence1_ _ [a] = a 
sequence1_ i (a:as) = a >> i >> sequence1_ i as 


-- | render a hoodle file to PDF simply
renderHoodleToPDF :: S.Hoodle -> FilePath -> IO () 
renderHoodleToPDF h ofp = do 
    let p = head (view S.pages h)
    let S.Dim width height = view S.dimension p  
    tdir <- getTemporaryDirectory
    uuid <- nextRandom
    let tempfile = tdir </> show uuid <.> "pdf"
    ctxt <- initRenderContext h
    withPDFSurface {- ofp -} tempfile width height $ \s -> 
      renderWith s . flip runStateT ctxt $
        sequence1_ (lift showPage) . map renderPage_StateT . view S.pages $ h 
    readProcessWithExitCode "pdftk" [ tempfile, "cat", "output", ofp ] ""
    return ()

isUpdated :: (FilePath,FilePath) -> IO Bool 
isUpdated (ofp,nfp) = do 
    b <- doesFileExist nfp
    if not b 
      then return True
      else do 
        otime <- getModificationTime ofp
        ntime <- getModificationTime nfp 
        return (otime > ntime)
  
-- | create pdf file with appropriate links
createPdf :: (String,String) -> FilePath -> (FilePath,FilePath) -> IO ()
createPdf (urlbase,specialurlbase) rootpath (fn,ofn) = catch action (\(e :: SomeException) -> print e)
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
                muuid = (fromString . B.unpack . view S.hoodleID) hdl
            -- print muuid
            tempfile <- (</>) <$> getTemporaryDirectory <*> liftM show nextRandom
            renderHoodleToPDF hdl tempfile
            --
            runPdfWriter ostr $ do 
              writePdfHeader
              deleteObject (Ref 0 65535) 0 
              flip evalStateT initialAppState $ do 
                index <- nextFreeIndex
                modify $ \st -> st { stRootNode = Ref index 0} 
                writePdfFile fn dim (urlbase,specialurlbase) (rootpath,currpath) tempfile npglnks muuid
                writeTrailer
            removeFile tempfile 
