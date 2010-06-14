import Data.List
import Graphics.Rendering.Hieroglyph
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BStr

sparkline :: Point -> Double -> Double -> [Double] -> Primitive
sparkline (Point startx starty) width height values = path{ begin=point0, segments=map Line points }
   where (point0:points) = zipWith Point xvals yvals
         xvals = iterate (+(width/n)) startx
         yvals = map (remap mx mn starty (starty+height)) values
         (mx,mn,_,_,_,n) = stats values

stats :: [Double] -> (Double,Double,Double,Double,Double,Double)
stats (x:xs) = finish . foldl' stats' (x,x,x,x*x,1) $ xs
stats' (mx,mn,s,ss,n) x = ( max x mx, min x mn, s + x, ss + x*x, n+1 )

finish (mx,mn,s,ss,n) = (mx,mn,av,va,stdev,n)
   where av = s/n
         va = ss/(n-1) - n*av*av/(n-1)
         stdev = sqrt va

remap :: Double -> Double -> Double -> Double -> Double -> Double
remap mx mn mx' mn' x = (x-mn) / (mx-mn) * (mx'-mn') + mn'

main = do
  [dataset, widthAsc, heightAsc, outputfile] <- getArgs
  values <- (map (read . BStr.unpack) . BStr.lines) `fmap` BStr.readFile dataset
  let width = read widthAsc
      height = read heightAsc
      visualization = (sparkline origin width height values){ attribs = whiteStroke }
      whiteStroke = plain{ strokeRGBA=white }
  renderToPNG outputfile (round width) (round height) visualization
