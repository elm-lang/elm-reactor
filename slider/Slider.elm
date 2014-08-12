module Slider where
{-| Slider input extension

# The slider
@docs slider

# The configuration
@docs defaultSlider, SliderStyle
-}

import Native.Slider
import Graphics.Input (Handle)

{-| The attributes of a slider. This lets you customize a slider to fit however
you want. You may also modify the default slider style with record updates.
-}
type SliderStyle = {
  horizontal : Bool,
  disabled : Bool,
  length : Int,
  min : Float,
  max : Float,
  step : Float,
  value : Float
 }

{-| The default range slider attributes. This is meant to be modified with record
updates to suite your needs.

      defaultStyle = {
        horizontal = True,
        disabled = False,
        length = 100,
        min = 0,
        max = 100,
        step = 1,
        value = 0
       }
-}
defaultSlider : SliderStyle
defaultSlider = {
  horizontal = True,
  disabled = False,
  length = 100,
  min = 0,
  max = 100,
  step = 1,
  value = 0
 }

 {-| Create a range slider. The following slider lets you choose your height in
meters with centimeter accuracy (0.01).

      height : Input Float
      height = input 1.6

      heightSlider : Element
      heightSlider =
          let ageStyle =
                  { defaultSlider
                  | horizontal <- False
                  , length <- 200
                  , min <- 1
                  , max <- 3
                  , step <- 0.01
                  }
          in  slider age.handle id ageStyle
-}
slider : Handle a -> (Float -> a) -> SliderStyle -> Element
slider = Native.Slider.slider
