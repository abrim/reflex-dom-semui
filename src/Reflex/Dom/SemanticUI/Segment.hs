{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}

module Reflex.Dom.SemanticUI.Segment where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------


data UiSegment = UiSegment
  { _uiSegment_style      :: Maybe UiSegmentStyle
  , _uiSegment_disabled   :: Maybe UiDisabled
  , _uiSegment_loading    :: Maybe UiLoading
  , _uiSegment_color      :: Maybe UiColor
  , _uiSegment_inverted   :: Maybe UiInverted
  , _uiSegment_attached   :: Maybe UiAttached
  , _uiSegment_padded     :: Maybe UiPadded
  , _uiSegment_compact    :: Maybe UiCompact
  , _uiSegment_emphasis   :: Maybe UiEmphasis
  , _uiSegment_circular   :: Maybe UiCircular
  , _uiSegment_custom     :: Maybe Text
  } deriving (Eq, Show)

instance Default UiSegment where
  def = UiSegment def def def def def def def def def def def

instance UiHasSegmentStyle UiSegment where
  uiSetSegmentStyle p s = s { _uiSegment_style = Just p }

instance UiHasDisabled UiSegment where
  disabled s = s { _uiSegment_disabled = Just UiDisabled }

instance UiHasLoading UiSegment where
  loading s = s { _uiSegment_loading = Just UiLoading }

instance UiHasColor UiSegment where
  uiSetColor c s = s { _uiSegment_color = Just c }

instance UiHasInverted UiSegment where
  inverted s = s { _uiSegment_inverted = Just UiInverted }

instance UiHasAttached UiSegment where
  uiSetAttached a s = s { _uiSegment_attached = Just a }

instance UiHasPadded UiSegment where
  uiSetPadded p s = s { _uiSegment_padded = Just p }

instance UiHasCompact UiSegment where
  compact s = s { _uiSegment_compact = Just UiCompact }

instance UiHasEmphasis UiSegment where
  uiSetEmphasis e s = s { _uiSegment_emphasis = Just e }

instance UiHasCircular UiSegment where
  circular s = s { _uiSegment_circular = Just UiCircular }

uiSegmentAttrs :: UiSegment -> Text
uiSegmentAttrs UiSegment{..} = T.unwords $ catMaybes
  [ uiText <$> _uiSegment_style
  , uiText <$> _uiSegment_disabled
  , uiText <$> _uiSegment_loading
  , uiText <$> _uiSegment_color
  , uiText <$> _uiSegment_inverted
  , uiText <$> _uiSegment_attached
  , uiText <$> _uiSegment_padded
  , uiText <$> _uiSegment_compact
  , uiText <$> _uiSegment_emphasis
  , uiText <$> _uiSegment_circular
  , _uiSegment_custom
  ]

uiSegment
  :: MonadWidget t m
  => Dynamic t UiSegment
  -> m a
  -> m a
uiSegment sDyn children = do
    elDynAttr "div" (mkAttrs <$> sDyn) children
  where
    mkAttrs i = "class" =: T.unwords ["ui", uiSegmentAttrs i, "segment"]
