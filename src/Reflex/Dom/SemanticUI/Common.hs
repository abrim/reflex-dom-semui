{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Common where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Reflex.Dom
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Temporary...will be moved out of here eventually.
tshow :: Show a => a -> Text
tshow = T.pack . show

------------------------------------------------------------------------------
-- | A type class for converting data types into appropriate  Semantic UI
-- class text.
class UiClassText a where
  uiText :: a -> Text

------------------------------------------------------------------------------
-- | Passthrough instance for Either
instance (UiClassText a, UiClassText b) => UiClassText (Either a b) where
  uiText (Left a) = uiText a
  uiText (Right b) = uiText b

class UiHasCustom a where
  -- | IMPORTANT: Implementations of this function should use the accompanying
  -- 'addCustom' function to make sure that new values are added on and don't
  -- overwrite anything that was already there.
  custom :: Text -> a -> a

------------------------------------------------------------------------------
-- | Helper function for adding a class item to a custom class field.
addCustom :: Text -> Maybe Text -> Maybe Text
addCustom cls Nothing = Just cls
addCustom cls (Just c) = Just (T.unwords [cls, c])

------------------------------------------------------------------------------
data UiColor
  = UiRed
  | UiOrange
  | UiYellow
  | UiOlive
  | UiGreen
  | UiTeal
  | UiBlue
  | UiViolet
  | UiPurple
  | UiPink
  | UiBrown
  | UiGrey
  | UiBlack
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiColor where
  uiText UiRed = "red"
  uiText UiOrange = "orange"
  uiText UiYellow = "yellow"
  uiText UiOlive = "olive"
  uiText UiGreen = "green"
  uiText UiTeal = "teal"
  uiText UiBlue = "blue"
  uiText UiViolet = "violet"
  uiText UiPurple = "purple"
  uiText UiPink = "pink"
  uiText UiBrown = "brown"
  uiText UiGrey = "grey"
  uiText UiBlack = "black"

class UiHasColor a where
  uiSetColor :: UiColor -> a -> a

instance (Reflex t, UiHasColor a) => UiHasColor (Dynamic t a) where
  uiSetColor c = fmap (uiSetColor c)

red, orange, yellow, olive, green, teal, blue, violet, purple, pink, brown, grey, black
  :: UiHasColor a => a -> a
red = uiSetColor UiRed
orange = uiSetColor UiOrange
yellow = uiSetColor UiYellow
olive = uiSetColor UiOlive
green = uiSetColor UiGreen
teal = uiSetColor UiTeal
blue = uiSetColor UiBlue
violet = uiSetColor UiViolet
purple = uiSetColor UiPurple
pink = uiSetColor UiPink
brown = uiSetColor UiBrown
grey = uiSetColor UiGrey
black = uiSetColor UiBlack

------------------------------------------------------------------------------
data UiEmphasis
  = UiPrimary
  | UiSecondary
  | UiTertiary
  | UiPositive
  | UiNegative
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiEmphasis where
  uiText UiPrimary = "primary"
  uiText UiSecondary = "secondary"
  uiText UiTertiary = "tertiary"
  uiText UiPositive = "positive"
  uiText UiNegative = "negative"

class UiHasEmphasis a where
  uiSetEmphasis :: UiEmphasis -> a -> a

primary, secondary, tertiary, positive, negative
  :: UiHasEmphasis a => a -> a
primary = uiSetEmphasis UiPrimary
secondary = uiSetEmphasis UiSecondary
tertiary = uiSetEmphasis UiTertiary
positive = uiSetEmphasis UiPositive
negative = uiSetEmphasis UiNegative

------------------------------------------------------------------------------
data UiBasic = UiBasic
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiBasic where
  uiText UiBasic = "basic"

class UiHasBasic a where
  basic :: a -> a

------------------------------------------------------------------------------
data UiInverted = UiInverted
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiInverted where
  uiText UiInverted = "inverted"

class UiHasInverted a where
  inverted :: a -> a

instance (Reflex t, UiHasInverted a) => UiHasInverted (Dynamic t a) where
  inverted = fmap inverted

------------------------------------------------------------------------------
data UiActive = UiActive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiActive where
  uiText UiActive = "active"

class UiHasActive a where
  active :: a -> a

------------------------------------------------------------------------------
data UiDisabled = UiDisabled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiDisabled where
  uiText UiDisabled = "disabled"

class UiHasDisabled a where
  disabled :: a -> a

------------------------------------------------------------------------------
data UiSize
  = UiMini
  | UiTiny
  | UiSmall
  | UiMedium
  | UiLarge
  | UiBig
  | UiHuge
  | UiMassive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiSize where
  uiText UiMini = "mini"
  uiText UiTiny = "tiny"
  uiText UiSmall = "small"
  uiText UiMedium = "medium"
  uiText UiLarge = "large"
  uiText UiBig = "big"
  uiText UiHuge = "huge"
  uiText UiMassive = "massive"

class UiHasSize a where
  uiSetSize :: UiSize -> a -> a

instance (Reflex t, UiHasSize a) => UiHasSize (Dynamic t a) where
  uiSetSize c = fmap (uiSetSize c)

mini, tiny, small, medium, large, big, huge, massive :: UiHasSize a => a -> a
mini = uiSetSize UiMini
tiny = uiSetSize UiTiny
small = uiSetSize UiSmall
medium = uiSetSize UiMedium
large = uiSetSize UiLarge
big = uiSetSize UiBig
huge = uiSetSize UiHuge
massive = uiSetSize UiMassive


------------------------------------------------------------------------------
data UiFlipped
  = UiFlipHoriz
  | UiFlipVert
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFlipped where
  uiText UiFlipHoriz = "horizontally flipped"
  uiText UiFlipVert = "vertically flipped"

class UiHasFlipped a where
  uiSetFlipped :: UiFlipped -> a -> a

flipHoriz, flipVert :: UiHasFlipped a => a -> a
flipHoriz = uiSetFlipped UiFlipHoriz
flipVert = uiSetFlipped UiFlipVert


------------------------------------------------------------------------------
data UiRotated
  = UiRotateCounterclockwise
  | UiRotateClockwise
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiRotated where
  uiText UiRotateCounterclockwise = "counterclockwise rotated"
  uiText UiRotateClockwise = "clockwise rotated"

class UiHasRotated a where
  uiSetRotated :: UiRotated -> a -> a

counterclockwise, clockwise :: UiHasRotated a => a -> a
counterclockwise = uiSetRotated UiRotateCounterclockwise
clockwise = uiSetRotated UiRotateClockwise


------------------------------------------------------------------------------
data UiAlignment
  = UiLeftAligned
  | UiCenterAligned
  | UiRightAligned
  | UiJustified
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiAlignment where
  uiText UiLeftAligned = "left aligned"
  uiText UiCenterAligned = "center aligned"
  uiText UiRightAligned = "right aligned"
  uiText UiJustified = "justified"

class UiHasAlignment a where
  uiSetAlignment :: UiAlignment -> a -> a

leftAligned, centerAligned, rightAligned, justified :: UiHasAlignment a => a -> a
leftAligned = uiSetAlignment UiLeftAligned
centerAligned = uiSetAlignment UiCenterAligned
rightAligned = uiSetAlignment UiRightAligned
justified = uiSetAlignment UiJustified


------------------------------------------------------------------------------
data UiFitted = UiFitted
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFitted where
  uiText UiFitted = "fitted"

class UiHasFitted a where
  fitted :: a -> a


------------------------------------------------------------------------------
data UiLeft = UiLeft
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLeft where
  uiText UiLeft = "left"

class UiHasLeft a where
  uiLeft :: a -> a
  -- Use the ui prefix to not clash with the left function from errors


------------------------------------------------------------------------------
data UiLoading = UiLoading
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLoading where
  uiText UiLoading = "loading"

class UiHasLoading a where
  loading :: a -> a


------------------------------------------------------------------------------
data UiCompact = UiCompact
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiCompact where
  uiText UiCompact = "compact"

class UiHasCompact a where
  compact :: a -> a


------------------------------------------------------------------------------
data UiToggle = UiToggle
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiToggle where
  uiText UiToggle = "toggle"

class UiHasToggle a where
  uiToggle :: a -> a


------------------------------------------------------------------------------
data UiFluid = UiFluid
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFluid where
  uiText UiFluid = "fluid"

class UiHasFluid a where
  fluid :: a -> a


------------------------------------------------------------------------------
data UiCircular = UiCircular
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiCircular where
  uiText UiCircular = "circular"

class UiHasCircular a where
  circular :: a -> a


------------------------------------------------------------------------------
data UiBordered = UiBordered
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiBordered where
  uiText UiBordered = "bordered"

class UiHasBordered a where
  bordered :: a -> a


------------------------------------------------------------------------------
data UiTransparent = UiTransparent
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiTransparent where
  uiText UiTransparent = "transparent"

class UiHasTransparent a where
  transparent :: a -> a


------------------------------------------------------------------------------
data UiImageStyle = UiImageStyle
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiImageStyle where
  uiText UiImageStyle = "image"

class UiHasImageStyle a where
  imageStyle :: a -> a


------------------------------------------------------------------------------
data UiPointing
  = UiPointingLeft
  | UiPointingRight
  | UiPointingUp
  | UiPointingDown
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiPointing where
  uiText UiPointingLeft = "left pointing"
  uiText UiPointingRight = "right pointing"
  uiText UiPointingUp = "pointing"
  uiText UiPointingDown = "pointing below"

class UiHasPointing a where
  uiSetPointing :: UiPointing -> a -> a

pointingLeft, pointingRight, pointingUp, pointingDown :: UiHasPointing a => a -> a
pointingLeft = uiSetPointing UiPointingLeft
pointingRight = uiSetPointing UiPointingRight
pointingUp = uiSetPointing UiPointingUp
pointingDown = uiSetPointing UiPointingDown


------------------------------------------------------------------------------
data UiLabelStyle
  = UiCornerLeft
  | UiCornerRight
  | UiTag
  | UiRibbonLeft
  | UiRibbonRight
  -- | UiLabelCircular
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLabelStyle where
  uiText UiCornerLeft = "left corner"
  uiText UiCornerRight = "right corner"
  uiText UiTag = "tag"
  uiText UiRibbonLeft = "left ribbon"
  uiText UiRibbonRight = "right ribbon"

class UiHasLabelStyle a where
  uiSetLabelStyle :: UiLabelStyle -> a -> a

cornerLeft, cornerRight, tag, ribbonLeft, ribbonRight
  :: UiHasLabelStyle a => a -> a

cornerLeft = uiSetLabelStyle UiCornerLeft
cornerRight = uiSetLabelStyle UiCornerRight
tag = uiSetLabelStyle UiTag
ribbonLeft = uiSetLabelStyle UiRibbonLeft
ribbonRight = uiSetLabelStyle UiRibbonRight


------------------------------------------------------------------------------
data UiAttached
  = UiAttachedLeft
  | UiAttachedRight
  | UiAttachedTop
  | UiAttachedBottom
  | UiAttached
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiAttached where
  uiText UiAttachedLeft = "left attached"
  uiText UiAttachedRight = "right attached"
  uiText UiAttachedTop = "top attached"
  uiText UiAttachedBottom = "bottom attached"
  uiText UiAttached = "attached"

class UiHasAttached a where
  uiSetAttached :: UiAttached -> a -> a

attachedLeft, attachedRight, attachedTop, attachedBottom, attached
  :: UiHasAttached a => a -> a
attachedLeft = uiSetAttached UiAttachedLeft
attachedRight = uiSetAttached UiAttachedRight
attachedTop = uiSetAttached UiAttachedTop
attachedBottom = uiSetAttached UiAttachedBottom
attached = uiSetAttached UiAttached


------------------------------------------------------------------------------
data UiHorizontal = UiHorizontal
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiHorizontal where
  uiText UiHorizontal = "horizontal"

class UiHasHorizontal a where
  horizontal :: a -> a


------------------------------------------------------------------------------
data UiFloating = UiFloating
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFloating where
  uiText UiFloating = "floating"

class UiHasFloating a where
  floating :: a -> a


------------------------------------------------------------------------------
data UiEmpty = UiEmpty
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiEmpty where
  uiText UiEmpty = "empty"

class UiHasEmpty a where
  empty :: a -> a


------------------------------------------------------------------------------
data UiLabeled = UiLabeled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLabeled where
  uiText UiLabeled = "labeled"

class UiHasLabeled a where
  labeled :: a -> a


------------------------------------------------------------------------------
data UiStatus
  = UiSuccess
  | UiWarning
  | UiError
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiStatus where
  uiText UiSuccess = "success"
  uiText UiWarning = "warning"
  uiText UiError = "error"

class UiHasStatus a where
  uiSetStatus :: UiStatus -> a -> a

hasSuccess, hasWarning, hasError :: UiHasStatus a => a -> a
hasSuccess = uiSetStatus UiSuccess
hasWarning = uiSetStatus UiWarning
hasError = uiSetStatus UiError


------------------------------------------------------------------------------
data UiEqualWidth = UiEqualWidth
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiEqualWidth where
  uiText UiEqualWidth = "equal width"

class UiHasEqualWidth a where
  equalWidth :: a -> a


------------------------------------------------------------------------------
data UiInline = UiInline
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiInline where
  uiText UiInline = "inline"

class UiHasInline a where
  inline :: a -> a


------------------------------------------------------------------------------
-- This needs to be improved
data UiWidth = UiWidth Int
  deriving (Eq,Ord,Read,Show,Bounded)

instance UiClassText UiWidth where
  uiText (UiWidth i) | i>0 && i<17 = w !! i <> " wide"
    where w = ["","one","two","three","four","five","six","seven","eight",
              "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
              "sixteen"]
  uiText _ = ""

instance Default UiWidth where
  def = UiWidth 1

class UiHasWidth a where
  width :: Int -> a -> a


------------------------------------------------------------------------------
data UiRequired = UiRequired
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiRequired where
  uiText UiRequired = "required"

class UiHasRequired a where
  required :: a -> a


------------------------------------------------------------------------------
data UiSegmentStyle
  = UiRaised
  | UiStacked
  | UiTallStacked
  | UiPiled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiSegmentStyle where
  uiText UiRaised = "raised"
  uiText UiStacked = "stacked"
  uiText UiTallStacked = "tall stacked"
  uiText UiPiled = "piled"

class UiHasSegmentStyle a where
  uiSetSegmentStyle :: UiSegmentStyle -> a -> a

raised, stacked, tallStacked, piled :: UiHasSegmentStyle a => a -> a
raised = uiSetSegmentStyle UiRaised
stacked = uiSetSegmentStyle UiStacked
tallStacked = uiSetSegmentStyle UiTallStacked
piled = uiSetSegmentStyle UiPiled

------------------------------------------------------------------------------
-- The SemanticUI docs are unclear as to whether vertical/horizonta (shown with
-- Grid) and very (shown with segment) are mutually exclusive
data UiPadded
  = UiPadded
  | UiVeryPadded
  | UiVerticallyPadded
  | UiHorizontallyPadded
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiPadded where
  uiText UiPadded = "padded"
  uiText UiVeryPadded = "very padded"
  uiText UiVerticallyPadded = "vertically padded"
  uiText UiHorizontallyPadded = "horizontally padded"

class UiHasPadded a where
  uiSetPadded :: UiPadded -> a -> a

padded, veryPadded, verticallyPadded, horizontallyPadded
  :: UiHasPadded a => a -> a
padded = uiSetPadded UiPadded
veryPadded = uiSetPadded UiVeryPadded
verticallyPadded = uiSetPadded UiVerticallyPadded
horizontallyPadded = uiSetPadded UiHorizontallyPadded

------------------------------------------------------------------------------
data UiDivided
  = UiDivided
  | UiVerticallyDivided
  | UiCelled
  | UiInternallyCelled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiDivided where
  uiText UiDivided = "divided"
  uiText UiVerticallyDivided = "vertically divided"
  uiText UiCelled = "celled"
  uiText UiInternallyCelled = "celled"

class UiHasDivided a where
  uiSetDivided :: UiDivided -> a -> a

divided, verticallyDivided, celled, internallyCelled :: UiHasDivided a => a -> a
divided = uiSetDivided UiDivided
verticallyDivided = uiSetDivided UiVerticallyDivided
celled = uiSetDivided UiCelled
internallyCelled = uiSetDivided UiInternallyCelled

------------------------------------------------------------------------------
data UiColumns = UiColumns Int
  deriving (Eq,Ord,Read,Show,Bounded)

instance UiClassText UiColumns where
  uiText (UiColumns i) | i>0 && i<17 = w !! i <> " column"
    where w = ["","one","two","three","four","five","six","seven","eight",
              "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
              "sixteen"]
  uiText _ = ""

instance Default UiColumns where
  def = UiColumns 1

class UiHasColumns a where
  columns :: Int -> a -> a

------------------------------------------------------------------------------
-- data UiFloated
--   = UiRightFloated
--   | UiLeftFloated
--   deriving (Eq,Ord,Read,Show,Enum,Bounded)
--
-- instance UiClassText UiFloated where
--   uiText UiRightFloated = "right floated"
--   uiText UiLeftFloated = "left floated"
--
-- class UiHasFloated a where
--   uiSetFloated :: UiFloated -> a -> a
--
-- leftFloated, rightFloated :: UiHasFloated a => a -> a
-- leftFloated = uiSetFloated UiLeftFloated
-- rightFloated = uiSetFloated UiRightFloated

------------------------------------------------------------------------------
data UiStretched = UiStretched
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiStretched where
  uiText UiStretched = "stretched"

class UiHasStretched a where
  stretched :: a -> a

------------------------------------------------------------------------------
data UiRelaxed = UiRelaxed
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiRelaxed where
  uiText UiRelaxed = "relaxed"

class UiHasRelaxed a where
  relaxed :: a -> a

------------------------------------------------------------------------------
data UiCentered = UiCentered
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiCentered where
  uiText UiCentered = "centered"

class UiHasCentered a where
  centered :: a -> a


------------------------------------------------------------------------------
data UiVerticalAlignment
    = UiTopAligned
    | UiMiddleAligned
    | UiBottomAligned
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiVerticalAlignment where
  uiText UiTopAligned = "top aligned"
  uiText UiMiddleAligned = "middle aligned"
  uiText UiBottomAligned = "bottom aligned"

class UiHasVerticalAlignment a where
  uiSetVerticalAlignment :: UiVerticalAlignment -> a -> a

topAligned, middleAligned, bottomAligned :: UiHasVerticalAlignment a => a -> a
topAligned = uiSetVerticalAlignment UiTopAligned
middleAligned = uiSetVerticalAlignment UiMiddleAligned
bottomAligned = uiSetVerticalAlignment UiBottomAligned


-------------------------------------------------------------------------------
-- Common styles for UiCustom elements

clearing, floatLeft, floatRight :: UiHasCustom a => a -> a
clearing = custom "clearing"
floatLeft = custom "left floated"
floatRight = custom "right floated"
