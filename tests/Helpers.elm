module Helpers exposing (..)

import Html.Attributes as Attr
import Test.Html.Selector as Selector exposing (Selector)


concourseGreen : String
concourseGreen =
    "#11c560"


white : String
white =
    "#fff"


isWhite : Selector
isWhite =
    attribute "fill" white


isConcourseGreen : Selector
isConcourseGreen =
    attribute "fill" concourseGreen


isConcourseRed : Selector
isConcourseRed =
    attribute "fill" "#ed4b35"


attribute : String -> String -> Selector
attribute name value =
    Selector.attribute <| Attr.attribute name value
