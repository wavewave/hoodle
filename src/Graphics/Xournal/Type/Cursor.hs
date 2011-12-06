module Graphics.Xournal.Type.Cursor where

data ListCtxt a = LCL [a] (ListCtxt a) 
                | LCR a (ListCtxt a)
                | LCTop

data AListCtxt a b = ALCL (AlterList b a) (AListCtxt b a)
                   | ALCR a (AListCtxt a b) 
                   | ALCTop
                   
mkListCtxt :: [a] -> Maybe (a, ListCtxt a)
mkListCtxt [] = Nothing 
mkListCtxt (x:xs) = Just (x, LCL xs LCTop)

nextL :: (a, ListCtxt a) -> Maybe (a, ListCtxt a)
nextL (a,(LCL [] ctxt)) = Nothing
nextL (a,(LCL (x:xs) ctxt)) = Just (x,(LCL xs (LCR a ctxt)) 

prevL :: (a, ListCtxt a) -> Maybe (a, ListCtxt a) 
prevL (a,(LCL xs LCTop)) = Nothing 
prevL (a,(LCL xs (LCR x ctxt)) = Just (x,(LCL (a:xs) ctxt))

mkAListCtxt :: AlterList a b -> Maybe (a, AListCtxt a b) 
mkAListCtxt [] = Nothing 
mkAListCtxt (x:-xs) = Just (x, ALCL xs ALCTop)
    

nextAL :: (a, AListCtxt a b) -> Maybe (b, AListCtxt b a)
nextAL (a,(ALCL Empty ctxt)) = Nothing
nextAL (a,(ALCL (x:-xs) ctxt)) = Just (x,ALCL xs (ALCR a ctxt))

prevAL :: (a, AListCtxt a b) -> Maybe (b, AListCtxt b a) 
prevAL (a,(ALCL xs ALCTop)) = Nothing 
prevAL (a,(ALCL xs (ALCR x ctxt))) = Just (x, ALCL (a:xs) ctxt)


{-


  
    ZList { prevs :: [a]
                          , current :: a 
                          , nexts :: [a] }
                    
data ZipperAlterList a b = ZALFront (ZipperList a) (AlterList [b] [a]) 
                         | ZALBack (AlterList [a] [b]) (ZipperAlterList b a)
                        
mkZipperList :: [a] -> ZipperList a                         
mkZipperList [] = error "cannot make a zipper list from empty list"
mkZipperList (x:xs) = ZList [] x xs

mkZipperAlterList :: AlterList [a] [b] -> ZipperAlterList a b 
mkZipperAlterList Empty = error "cannot make a zipper alterlist from empty"
mkZipperAlterList x :- xs = ZListA Empty (mkZipperList x) xs 

zlistNext :: ZipperList a -> Maybe (ZipperList a)
zlistNext (ZList ps c []) = Nothing 
zlistNext (ZList ps c (n:ns)) = Just (ZList (c:ps) n ns)

zlistPrev :: ZipperList a -> Maybe (ZipperList a)
zlistPrev (ZList [] c ns) = Nothing 
zlistPrev (ZList (p:ps) c ns) = Just (ZList ps p (c:ns))

zalistNext :: ZipperAlterList a b -> Maybe (ZipperAlterList a b)
zalistNext (ZAListA alprev cl alnext) = 
  case zlistNext cl of 
    Just cl' -> ZAListA alprev cl' alnext
    Nothing -> (prevs cl)
-}