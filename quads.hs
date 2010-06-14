module Quads where

	data Quad a b = Quad a a b b

	firstTwo :: (Quad a b) -> [a]
	firstTwo (Quad x y _ _) = x:y:[]

	lastTwo :: (Quad a b) -> [b]
	lastTwo (Quad _ _ x y) = x:y:[]
