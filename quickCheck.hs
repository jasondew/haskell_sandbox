module Main where

import System
import System.Exit
import List

main :: IO ()
main = do as <- getArgs
          sequence_ (map (process (filter isOption as)) 
	                  (filter (not.isOption) as))
          exitSuccess

process opts file =
       let (namesOpt,opts') = getOption "names" "-names" opts in
       do xs<-readFile file
          let names = nub$ filter ("prop_" `isPrefixOf`)
	                (map (fst.head.lex) (lines xs))
          if null names then
	      putStr (file++": no properties to check\n")
	    else do writeFile "ghcin"$
	              unlines ((":l "++file):
	                       [(if namesOpt=="+names" then 
			           "putStr \""++p++": \" >> "
				 else "") ++
				"quickCheck "++p | p<-names])
	            system ("ghci "++options opts'++" < ghcin")
	            return ()

isOption xs = head xs `elem` "-+"

options opts = unwords ["\""++opt++"\"" | opt<-opts]

getOption name def opts = 
  let opt = head [opt | opt<-opts++[def], isPrefixOf name (drop 1 opt)] in
    (opt, filter (/=opt) opts)
