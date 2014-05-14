{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc.Builder
import Text.Pandoc
import Data.Monoid ((<>), mempty)
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Station = Station{
    address        :: String
  , name           :: String
  , cardsAccepted  :: [String]
  } deriving Show

instance FromJSON Station where
    parseJSON (Object v) = Station <$>
       v .: "street_address" <*>
       v .: "station_name" <*>
       (words <$> (v .:? "cards_accepted" .!= ""))
    parseJSON _          = mzero

createLetter :: [Station] -> Pandoc
createLetter stations = doc $
    para "Dear Boss:" <>
    para "Here is a list of CNG stations that accept Voyager cards." <>
    orderedList (map toStationItem stations) <>
    para "Your loyal servant," <>
    plain (image "JohnHancock.png" "" mempty)
  where
    toStationItem station = para $
      strong (text (name station)) <>
      ", " <> text (address station) <> "."

main :: IO ()
main = do
  json <- BL.getContents
  let letter = case decode json of
                    Just stations -> createLetter [s | s <- stations,
                                        "Voyager" `elem` cardsAccepted s]
                    Nothing       -> error "Could not decode JSON"
  BL.writeFile "letter.docx" =<< writeDocx def letter
