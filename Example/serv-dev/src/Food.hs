
{-# LANGUAGE TemplateHaskell #-}

module Food where

import           Database.Persist.TH

-- This is in a separate file b/c template haskell has trouble
-- generating this in the same file it's used...

data Food = ALL | BEEF | CHK | FISH | HAM | MCH | MTL | SPG | TUR
    deriving (Show, Ord, Eq, Read)
derivePersistField "Food"
