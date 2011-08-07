type Vector = [Double]

(<+>) :: Vector -> Vector -> Vector
a <+> b = map (\(x, y) -> x + y) $ a `zip` b

(<->) :: Vector -> Vector -> Vector
a <-> b = map (\(x, y) -> x - y) $ a `zip` b

(<|>) :: Vector -> Vector -> Double
a <|> b = sum . map (\(x, y) -> x * y) $ a `zip` b

(<*>) :: Double -> Vector -> Vector
s <*> v = map (* s) v

(</>) :: Vector -> Double -> Vector
v </> s = (1.0 / s) <*> v

norm :: Vector -> Double
norm x = sqrt $ x <|> x

normalize :: Vector -> Vector
normalize x = x </> (norm x)

gram_schmidt :: [Vector] -> [Vector]
gram_schmidt [] = []
gram_schmidt bs = gram_schmidt' bs []
  where gram_schmidt' []     ys = ys
        gram_schmidt' (b:bs) ys = let zero   = replicate (length b) 0
                                      offset = foldr (<+>) zero $ map (\v -> v <|> b <*> v) ys
                                      y      = normalize $ b <-> offset
                                  in gram_schmidt' bs (y:ys)

-- manual procedure
b_1 = [-3,0,4] :: [Double]
b_2 = [3,-1,2] :: [Double]
b_3 = [0,1,-1] :: [Double]
bs = [b_1, b_2, b_3]

x_1 = b_1
y_1 = normalize x_1

x_2 = b_2 <-> (y_1 <|> b_2 <*> y_1)
y_2 = normalize y_2

x_3 = b_3 <-> (y_2 <|> b_3 <*> y_2) <-> (y_1 <|> b_3 <*> y_1)
y_3 = normalize x_3
