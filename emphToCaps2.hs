import Text.Pandoc.JSON
import Text.Pandoc.Walk
import AllCaps (allCaps)

emphToCaps :: Maybe Format -> Inline -> [Inline]
emphToCaps (Just f) (Emph xs)
  | f == Format "html" || f == Format "latex" = [SmallCaps xs]
emphToCaps _ (Emph xs) = walk allCaps xs
emphToCaps _ x = [x]

main :: IO ()
main = toJSONFilter emphToCaps
