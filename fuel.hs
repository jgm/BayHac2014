{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc.Builder
import Text.Pandoc
import Data.Monoid ((<>), mempty, mconcat)
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.List (intersperse)

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
    para "Here are the CNG stations that accept Voyager cards:" <>
    simpleTable [plain "Station", plain "Address", plain "Cards accepted"]
           (map stationToRow stations) <>
    para "Your loyal servant," <>
    plain (image "JohnHancock.png" "" mempty)
  where
    stationToRow station =
      [ plain (text $ name station)
      , plain (text $ address station)
      , plain (mconcat $ intersperse linebreak $ map text $ cardsAccepted station)
      ]
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
