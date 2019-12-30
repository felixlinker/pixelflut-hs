module Lib
    ( payloads
    , pxs
    , PX (..)
    )
where

import           Codec.Picture.Png
import           Codec.Picture.Types
import           Control.Exception
import           Text.Bytedump
import           Text.Printf
-- import           System.Random
-- import           System.Random.Shuffle
import qualified Data.ByteString.Char8         as B

data PX = PX Int Int String
instance Show PX where
    show (PX x y hex) = printf "PX %d %d %s\n" x y hex

shift :: Int -> Int -> PX -> PX
shift dx dy (PX x y c) = PX (x + dx) (y + dy) c

rgb8ToHex :: Pixel8 -> Pixel8 -> Pixel8 -> String
rgb8ToHex r g b = (foldl1 (++) . map hexString) [r, g, b]

payloads :: Int -> Int -> B.ByteString -> [B.ByteString]
payloads dx dy = map (B.pack . show . shift dx dy) . pxs

data E = E String
instance Show E where
    show (E s) = s
instance Exception E

pxs :: B.ByteString -> [PX]
pxs raw =
    let dimg = decodePng raw
    in  case dimg of
            Left  err             -> throw $ E err
            Right (ImageRGB8 img) -> pixelFold
                (\acc x y (PixelRGB8 r g b) -> (PX x y $ rgb8ToHex r g b):acc)
                []
                img
            Right _ -> throw $ E "Don't know image format"
