import Data.Bits
import Graphics.GD

alpha :: Num a => Color -> a
alpha color = fromIntegral $ color `shiftR` 24

red :: Num a => Color -> a
red color = fromIntegral $ (color .&. 16711680) `shiftR` 16

green :: Num a => Color -> a
green color = fromIntegral $ (color .&. 65280) `shiftR` 8

blue :: Num a => Color -> a
blue color = fromIntegral $ color .&. 255

intensityTransform :: (Color -> Color) -> Image -> IO Image
intensityTransform f image = do
  (width, height) <- imageSize image
  let points = [(x, y) | x <- [1..width], y <- [1..height]]
  mapM (pixelTransform f image) points
  return image

pixelTransform :: (Color -> Color) -> Image -> Point -> IO ()
pixelTransform f image pixel = do
  color <- getPixel pixel image
  setPixel pixel (f color) image

colorTransform :: (Int -> Int) -> (Color -> Color)
colorTransform f c = rgb (f $ red c) (f $ green c) (f $ blue c)

logTransform :: Color -> Color
logTransform = colorTransform (\x -> round $ c * (log . fromIntegral $ x + 1))
               where c = 256 / log 256

negativeTransform :: Color -> Color
negativeTransform = colorTransform (\x -> 255 - x)

powerTransform :: Double -> Color -> Color
powerTransform g = colorTransform (\x -> round $ c * ((fromIntegral x) ** g))
                   where c = 256 / 256 ** g

brighten :: Int -> Color -> Color
brighten v = colorTransform (\x -> minimum [x + v, 255])

main = do
  image  <- loadJpegFile("monalisa.jpg")
  image' <- intensityTransform (brighten 50) image
  savePngFile "monalisa-prime.png" image'
