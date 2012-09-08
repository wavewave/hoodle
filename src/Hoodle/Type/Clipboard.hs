{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Clipboard 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Clipboard where

import           Control.Category
import           Control.Lens
import           Control.Lens.TH
-- from hoodle-platform
import           Data.Hoodle.BBox
--
import Prelude hiding ((.), id)

-- |

newtype Clipboard = Clipboard { unClipboard :: [StrokeBBox] }

-- |

emptyClipboard :: Clipboard
emptyClipboard = Clipboard []

-- |

isEmpty :: Clipboard -> Bool 
isEmpty = null . unClipboard 

-- |

getClipContents :: Clipboard -> [StrokeBBox] 
getClipContents = unClipboard

-- |

replaceClipContents :: [StrokeBBox] -> Clipboard -> Clipboard
replaceClipContents strs _ = Clipboard strs 

-- |

data SelectType = SelectRegionWork 
                | SelectRectangleWork 
                | SelectVerticalSpaceWork
                | SelectHandToolWork 
                deriving (Show,Eq,Ord) 

-- |

data SelectInfo = SelectInfo { _selectType :: SelectType
                             }
             deriving (Show) 

makeLenses ''SelectInfo
