{-# LANGUAGE TemplateHaskell #-}

-- This module is taken from https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html

module Derive where

import Language.Haskell.TH
import Control.Monad

data T1 = T1
data T2 a = T2 a

deriveShow t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ _ constructors _)  <-  reify t

  -- Make `show` clause for one constructor:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  let showClause (NormalC name fields) = do
        -- Name of constructor, i.e. "A". Will become string literal in generated code
        let constructorName = replaceAll replacements $ nameBase name

        -- Get variables for left and right side of function definition
        (pats,vars) <- genPE (length fields)
        -- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
        let f []       = [| "" |]
            f (v:vars) = [| " " ++ showAndReplace $v ++ $(f vars) |]
        -- Generate function clause for one constructor

        clause [conP name pats]                                 -- (A x1 x2)
               (normalB [| constructorName ++ $(f vars) |]) []  -- "A "++show x1++" "++show x2

  -- Make body for function `show`:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  --   show (B x1)    = "B "++show x1
  --   show C         = "C"
  showbody <- mapM showClause constructors

  -- Generate template instance declaration and then replace
  --   type name (T1) and function body (\x -> "text") with our data
  d <- [d| instance Show T1 where
             show x = "text"
       |]
  let    [InstanceD Nothing [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD Nothing [] (AppT showt (ConT t  )) [FunD showf showbody]]


-- Generate n unique variables and return them in form of patterns and expressions
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)


--------------------------------------------------------------------------------

showAndReplace :: (Show a) => a -> String
showAndReplace = replaceAll replacements . show

replaceAll :: [(String,String)] -> String -> String
replaceAll []            haystack = haystack
replaceAll ((n,r):ns_rs) haystack = replace n r (replaceAll ns_rs haystack)

replacements = 
  [ ("_dash_",  "-")
  , ("_slash_", "/") 
  , ("_plus_", "+")
  , ("_minus_", "-")

  , ("Sin_", "@")
  , ("_sin_", "@")

  -- Eskuineko ("on the right of") points to the left <, 
  -- because the word itself is on the right. Same for Ezkerreko.
  , ("_esk_", "<")
  , ("_ezk_", ">")

  , ("Nothing", "")
  , ("Just ", "")
  , ("Left ", "")
  , ("Right ", "")
  , ("_n_", "")
  , ("_v_", "")
  , ("(",   "")
  , (")",   "")
  , (",",  " ")
  ]
-- This function is taken from StackOverflow
-- http://stackoverflow.com/questions/14880299/how-can-i-replace-a-substring-of-a-string-with-another-in-haskell-without-using

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack
  = case begins haystack needle of
      Just remains -> replacement ++ remains
      Nothing      -> case haystack of
                        []     -> []
                        x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack []                = Just haystack
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _                 = Nothing