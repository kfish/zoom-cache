
module Main (
    main
) where

import Control.Monad.Trans (liftIO)
import Data.Default
import UI.Command

------------------------------------------------------------

zoomHello = defCmd {
          cmdName = "hello"
        , cmdHandler = zoomHelloHandler
        , cmdCategory = "Reporting"
        , cmdShortDesc = "Say Hello"
        , cmdExamples = [("Yo", "")]
        }

zoomHelloHandler = liftIO . putStrLn $ "Hello world!"

------------------------------------------------------------
-- The Application
--

zoom :: Application () ()
zoom = def {
          appName = "zoom"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial zoom inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reporting"]
        , appSeeAlso = [""]
        , appProject = "Zoom"
        , appCmds = [zoomHello]
	}

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
