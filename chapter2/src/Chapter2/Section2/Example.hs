{-# LANGUAGE UnicodeSyntax #-}
module Chapter2.Section2.Example where

firstOrEmpty lst = if not (null lst) then head lst  else  "empty"


