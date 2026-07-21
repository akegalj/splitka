{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------
import Miso
import qualified Miso.Html as H
import Miso.Lens

----------------------------------------------------------------------------

-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)

-- NOTE: see https://github.com/haskell-miso/sampler/blob/main/app/Main.hs

----------------------------------------------------------------------------

-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------

-- | `vcomp` takes as arguments the initial model, update function, view function
app :: App Int Action
app = vcomp 0 updateModel viewModel

-- app = (component 0 updateModel viewModel)
--   { styles = [ Sheet sheet ]
--   }

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent props Int Action
updateModel = \case
  AddOne -> this += 1
  SubtractOne -> this -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"

----------------------------------------------------------------------------

-- | Constructs a virtual DOM from a model
viewModel :: () -> Int -> View Int Action
viewModel _props x =
  vfrag
    [ H.button_ [H.onClick AddOne] [text "+"]
    , text (ms x)
    , H.button_ [H.onClick SubtractOne] [text "-"]
    , H.br_ []
    , H.button_ [H.onClick SayHelloWorld] [text "Alert Hello World!!!!"]
    ]

----------------------------------------------------------------------------