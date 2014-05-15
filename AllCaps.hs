module AllCaps (allCaps) where
import Text.Pandoc.Definition
import Data.Char (toUpper)

allCaps :: Inline -> Inline
allCaps (Str xs) = Str $ map toUpper xs
allCaps x = x
