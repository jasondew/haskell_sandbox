module Stats where

	mean [] = null
	mean x = (sum x) / (length x)
