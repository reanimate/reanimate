module Common
  ( monalisa
  , monalisaLarge
  , applyColorMap
  )
where

import qualified Data.ByteString               as BS
import           Codec.Picture
import           System.IO.Unsafe

monalisa :: Image PixelRGB8
monalisa = unsafePerformIO $ do
  dat <- BS.readFile "monalisa.jpg"
  case decodeJpeg dat of
    Left  err -> error err
    Right img -> return $ convertRGB8 img

monalisaLarge :: Image PixelRGB8
monalisaLarge = scaleImage 15 monalisa

scaleImage :: Pixel a => Int -> Image a -> Image a
scaleImage factor img = generateImage fn
                                      (imageWidth img * factor)
                                      (imageHeight img * factor)
  where fn x y = pixelAt img (x `div` factor) (y `div` factor)

applyColorMap :: (Double -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
applyColorMap cmap img = generateImage fn (imageWidth img) (imageHeight img)
 where
  fn x y = case pixelAt img x y of
    PixelRGB8 r _ _ -> cmap (fromIntegral r / 255)
