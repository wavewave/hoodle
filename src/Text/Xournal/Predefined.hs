module Text.Xournal.Predefined where

import qualified Data.Map as M


hexToRGBA :: Integer -> (Double,Double,Double,Double) 
hexToRGBA n = 
  let r = n `div` (256*256*256)
      g = (n-r*256*256*256) `div` (256*256)
      b = (n-r*256*256*256-g*256*256) `div` 256 
      a = n-r*256*256*256-g*256*256-b*256  
  in  (fromIntegral r/255.0,fromIntegral g/255.0,fromIntegral b/255.0,fromIntegral a/255.0)


predefined_pencolor :: M.Map String (Double,Double,Double,Double)
predefined_pencolor = 
  M.fromList [ ("black"     , hexToRGBA 0x000000ff)
             , ("blue"      , hexToRGBA 0x3333ccff)
             , ("red"       , hexToRGBA 0xff0000ff)
             , ("green"     , hexToRGBA 0x008000ff)
             , ("gray"      , hexToRGBA 0x808080ff)
             , ("lightblue" , hexToRGBA 0x00c0ffff)
             , ("lightgreen", hexToRGBA 0x00ff00ff)
             , ("magenta"   , hexToRGBA 0xff00ffff)
             , ("orange"    , hexToRGBA 0xff8000ff)
             , ("yellow"    , hexToRGBA 0xffff00ff)
             , ("white"     , hexToRGBA 0xffffffff) ] 


predefined_bkgcolor :: M.Map String (Double,Double,Double,Double)
predefined_bkgcolor = 
  M.fromList [ (""      , hexToRGBA 0xffffffff) 
             , ("blue"  , hexToRGBA 0xa0e8ffff)
             , ("pink"  , hexToRGBA 0xffc0d4ff)
             , ("green" , hexToRGBA 0x80ffc0ff)
             , ("orange", hexToRGBA 0xffc080ff)
             , ("yellow", hexToRGBA 0xffff80ff)
             , ("white" , hexToRGBA 0xffffffff) ]




predefined_RULING_MARGIN_COLOR = hexToRGBA 0xff0080ff
predefined_RULING_COLOR = hexToRGBA 0x40a0ffff
predefined_RULING_THICKNESS = 0.5
predefined_RULING_LEFTMARGIN = 72.0
predefined_RULING_TOPMARGIN = 80.0
predefined_RULING_SPACING = 24.0
predefined_RULING_BOTTOMMARGIN = predefined_RULING_SPACING
predefined_RULING_GRAPHSPACING = 14.17
