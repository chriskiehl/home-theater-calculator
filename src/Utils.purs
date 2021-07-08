module Utils where 

import Prelude

import CanvasSupport (getBoundingClientRect)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (fromJust)
import Debug (spy)
import Partial.Unsafe (unsafePartial)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import Types (CanvasPosition, Position, LocalPosition)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (DOMRect)


unsafeLookup :: forall k v. Ord k => k -> Map.Map k v -> v 
unsafeLookup k m = unsafePartial $ fromJust $ Map.lookup k m 


getValue :: EventFn SyntheticEvent String 
getValue = unsafeEventFn \e -> (unsafeCoerce e).target.value

clientPosition :: EventFn SyntheticEvent {clientX :: Int, clientY :: Int}
clientPosition = unsafeEventFn \e -> {clientX: (unsafeCoerce e).clientX, clientY: (unsafeCoerce e).clientY}


-- | Extracts the client (i.e. cursor) position as well as the evenet 
-- | target's boundingRect and gives back a position local to the supplied element. 
-- | In practice, this transforms events such that (0,0) maps to the top-left of the canvas
-- | rather than the screen  
canvasPosition :: EventFn SyntheticEvent LocalPosition
canvasPosition = unsafeEventFn \e -> 
  let rect = getBoundingClientRect (unsafeCoerce e).target 
      client = {x: toNumber (unsafeCoerce e).clientX, y: toNumber (unsafeCoerce e).clientY}
  in {x: client.x - rect.left, y: client.y - rect.top} 
  

