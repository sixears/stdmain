{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- if they compile, they work
import StdMain               ()
import StdMain.OptionsTools  ()
import StdMain.StdOptions    ()
import StdMain.UsageError    ()

import StdMain.VerboseOptions  ( tests )

--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests

-- that's all, folks! ----------------------------------------------------------
