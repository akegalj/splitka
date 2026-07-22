{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------
import Miso
import Miso.Html (body_, br_, button_, div_, doctype_, head_, html_, link_, meta_, onClick, span_, title_, style_)
import Miso.Html.Property (charset_, class_, classes_, content_, href_, name_, rel_)
import Miso.Lens
import CSS.Bulma (bulma_1_0_4)

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

--    { -- FIXME: use Sheet instead and convert from css into Sheet
--      styles = [Href "https://cdn.jsdelivr.net/gh/pruger/tiny-brutalism-css/tiny-brutalism.css" True]
--    }

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
    [ doctype_
    , html_
        []
        [ head_ [] headView
        , body_ [] bodyView
        ]
    ]
 where
  headView=
    [ meta_ [charset_ "utf8"]
    , meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    , title_ [] ["Splitka"]
    , style_ [] bulma_1_0_4
    ]
  bodyView =
    [ button_ [class_ "button"] ["bok"]
    , div_ [classes_ ["columns", "is-mobile"]]
        [ div_ [classes_ ["column", "is-1"]] ["1"]
        , div_ [classes_ ["column", "is-1"]] ["1"]
        , div_ [classes_ ["column", "is-9"]] ["1"]
        , div_ [classes_ ["column", "is-1", "has-text-right"]] ["1"]
        ]
    ]

----------------------------------------------------------------------------