{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.SemanticUI.Label where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------

data UiLabel = UiLabel
  { _uiLabel_basic      :: Maybe UiBasic
  , _uiLabel_imageStyle :: Maybe UiImageStyle
  , _uiLabel_pointing   :: Maybe UiPointing
  , _uiLabel_style      :: Maybe UiLabelStyle
  , _uiLabel_attached   :: Maybe UiAttached
  , _uiLabel_horizontal :: Maybe UiHorizontal
  , _uiLabel_floating   :: Maybe UiFloating
  , _uiLabel_empty      :: Maybe UiEmpty
  , _uiLabel_circular   :: Maybe UiCircular
  , _uiLabel_color      :: Maybe UiColor
  , _uiLabel_size       :: Maybe UiSize
  , _uiLabel_custom     :: Maybe Text
  } deriving (Eq, Show)

instance Default UiLabel where
  def = UiLabel def def def def def def def def def def def def

instance UiHasBasic UiLabel where
  basic l = l { _uiLabel_basic = Just UiBasic }

instance UiHasImageStyle UiLabel where
  imageStyle l = l { _uiLabel_imageStyle = Just UiImageStyle }

instance UiHasPointing UiLabel where
  uiSetPointing p l = l { _uiLabel_pointing = Just p }

instance UiHasLabelStyle UiLabel where
  uiSetLabelStyle s l = l { _uiLabel_style = Just s }

instance UiHasAttached UiLabel where
  uiSetAttached a l = l { _uiLabel_attached = Just a }

instance UiHasHorizontal UiLabel where
  horizontal l = l { _uiLabel_horizontal = Just UiHorizontal }

instance UiHasFloating UiLabel where
  floating l = l { _uiLabel_floating = Just UiFloating }

instance UiHasEmpty UiLabel where
  empty l = l { _uiLabel_empty = Just UiEmpty }

instance UiHasCircular UiLabel where
   circular l = l { _uiLabel_circular = Just UiCircular }

instance UiHasColor UiLabel where
  uiSetColor c l = l { _uiLabel_color = Just c }

instance UiHasSize UiLabel where
  uiSetSize s l = l { _uiLabel_size = Just s }

uiLabelAttrs :: UiLabel -> Text
uiLabelAttrs UiLabel{..} = T.unwords $ catMaybes
  [ uiText <$> _uiLabel_floating
  , Just "ui"
  , uiText <$> _uiLabel_imageStyle
  , uiText <$> _uiLabel_pointing
  , uiText <$> _uiLabel_style
  , uiText <$> _uiLabel_attached
  , uiText <$> _uiLabel_horizontal
  , uiText <$> _uiLabel_empty
  , uiText <$> _uiLabel_color
  , uiText <$> _uiLabel_size
  , _uiLabel_custom
  , Just "label"
  ]

uiLabel
  :: MonadWidget t m
  => Dynamic t UiLabel
  -> m a
  -> m a
uiLabel lDyn children = do
    elDynAttr "label" (mkAttrs <$> lDyn) children
  where
    mkAttrs i = "class" =: uiLabelAttrs i
