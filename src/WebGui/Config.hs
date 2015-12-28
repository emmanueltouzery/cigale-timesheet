module Config where

import Reflex.Dom

import Common

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    attrsDyn <- mapDyn (\curView -> styleWithHideIf (curView /= ActiveViewConfig) "height: 100%;") activeViewDyn
    elDynAttr "div" attrsDyn $
        text "Config... TODO!"
