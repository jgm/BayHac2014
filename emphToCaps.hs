-- emphToCaps.hs
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import AllCaps (allCaps)

emphToCaps :: Inline -> Inline
emphToCaps (Emph xs) = Emph $ walk allCaps xs
emphToCaps x = x

main :: IO ()
main = toJSONFilter emphToCaps
