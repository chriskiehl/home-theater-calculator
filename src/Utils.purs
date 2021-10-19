module Utils where 

import Prelude

import CanvasSupport (getBoundingClientRect)
import Constants (canvasWidth)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Partial.Unsafe (unsafePartial)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import Types (LocalPosition)
import Unsafe.Coerce (unsafeCoerce)


getValue :: EventFn SyntheticEvent String 
getValue = unsafeEventFn \e -> (unsafeCoerce e).target.value

clientPosition :: EventFn SyntheticEvent {clientX :: Int, clientY :: Int}
clientPosition = unsafeEventFn \e -> {clientX: (unsafeCoerce e).clientX, clientY: (unsafeCoerce e).clientY}


-- | Extracts the client (i.e. cursor) position as well as the evenet 
-- | target's boundingRect and gives back a position local to the supplied element. 
-- | In practice, this transforms events such that (0,0) maps to the top-left of the canvas
-- | rather than the screen  
-- | NOTE: Canvas pixels != Screen pixels. Canvas pixels are **scaled**. They're based on the 
-- | specified size of the canvas element, not its physical size in the DOM. As such, we scale 
-- | the pixel locations reported by the browser relative to the current size of the canvas 
canvasPosition :: EventFn SyntheticEvent (Effect LocalPosition)
canvasPosition = unsafeEventFn \e -> do 
  canvasWidth <- Canvas.getCanvasWidth (unsafeCoerce e).target 
  canvasHeight <- Canvas.getCanvasHeight (unsafeCoerce e).target 
  let rect = getBoundingClientRect (unsafeCoerce e).target 
      scaleX = canvasWidth / rect.width 
      scaleY = canvasHeight / rect.height 
      client = {x: toNumber (unsafeCoerce e).clientX, y: toNumber (unsafeCoerce e).clientY}
  pure {x: (client.x - rect.left) * scaleX, y: (client.y - rect.top) * scaleY} 
  

-- | combines two boolean functions into a new function testing both cases
--   Another thing which surely already exists, but I can't find :|  
booleanAnd :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean) 
booleanAnd f g = (\s -> (f s) && (g s))

booleanOr :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean) 
booleanOr f g = (\s -> (f s) || (g s))

infixr 5 booleanAnd as &&&&
infixr 5 booleanOr as ||||


