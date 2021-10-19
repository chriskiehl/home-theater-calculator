-- | Additional supporting methods for patching around 
-- | some missing functionality in the purescript-canvas library
module CanvasSupport where

import Prelude
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (CanvasElement, CanvasImageSource(..), Context2D, getCanvasElementById, getContext2D)
import Prelude (Unit, bind, pure, ($))
import Web.HTML.HTMLElement (DOMRect)

foreign import fromDataSource :: Fn1 String CanvasImageSource

foreign import getBoundingClientRect_ :: Fn1 CanvasElement DOMRect

foreign import removaAllListeners :: Int -> Effect Unit

foreign import imageSmoothingEnabled :: Context2D -> Boolean -> Effect Unit 



getBoundingClientRect :: CanvasElement -> DOMRect 
getBoundingClientRect elm = runFn1 getBoundingClientRect_ elm 


type Environment = {
    canvas :: CanvasElement, 
    ctx :: Context2D
}


getCanvasEnvironment :: String -> Effect (Maybe Environment)
getCanvasEnvironment id = do 
  maybeCanvas <- (getCanvasElementById id)
  case maybeCanvas of 
    Nothing -> pure Nothing 
    Just canvas -> do 
      ctx <- getContext2D canvas 
      _ <- imageSmoothingEnabled ctx false
      pure $ Just {canvas, ctx}
  
