{-# LANGUAGE OverloadedStrings #-}
import           Clay

test :: Css
test = body ? color red 

main :: IO ()
main = putCss test
