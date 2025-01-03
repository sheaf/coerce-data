module Main where

import Framework
  ( Test(..), runTests )
import Tests

--------------------------------------------------------------------------------

tests :: [ Test ]
tests =
  [ Test "A1/A2" ( a1 == ca1 && a2 == ca2 ) True
  , Test "B1/B2" ( b1 == cb1 && b2 == cb2 ) True
  , Test "C1/C2" ( c1 == cc1 && c2 == cc2 ) True
  , Test "E1/E2" ( e1 == ce1 && e2 == ce2 ) True
  , Test "F1/F2" ( f1 == cf1 && f2 == cf2 ) True
  ]

main :: IO ()
main = runTests tests
