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
import           System.Random
import           System.Random.Shuffle
import qualified Data.ByteString.Char8         as B
import qualified Data.Map                      as Map

data PX = PX
    { x :: Int
    , y :: Int
    , hex :: String }
instance Show PX where
    show (PX x y hex) = printf "PX %d %d %s\n" x y hex

shift :: Int -> Int -> PX -> PX
shift dx dy (PX x y c) = PX (x + dx) (y + dy) c

randomize :: [PX] -> [PX]
-- Initialize mkStdGen with 0 because true randomness is not important
randomize ps = shuffle' ps (length ps) $ mkStdGen 0

countColors :: Fractional frc => [PX] -> Map.Map String frc
countColors = foldl (\acc v -> Map.insertWith (+) (hex v) 1 acc) Map.empty

relMap :: (Ord ord, Fractional frc) => Map.Map ord frc -> Map.Map ord frc
relMap m = let sum = Map.foldl (+) 0 m in fmap (/ sum) m

inflate :: [PX] -> [PX]
inflate ps =
    let colorRelFreq = (relMap . countColors) ps
    in  concat $ map
            (\(PX x y hex) ->
                let freq = colorRelFreq Map.! hex
                in  replicate (ceiling $ 1 / freq) (PX x y hex)
            )
            ps

rgb8ToHex :: Pixel8 -> Pixel8 -> Pixel8 -> String
rgb8ToHex r g b = (foldl1 (++) . map hexString) [r, g, b]

payloads :: Int -> Int -> B.ByteString -> [B.ByteString]
payloads dx dy = map (B.pack . show . shift dx dy) . randomize . pxs

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
