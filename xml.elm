
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Xml.Decode exposing (..)

type alias Data =
    { string : String
    , integers : List Int
    }

dataDecoder : Decoder Data
dataDecoder =
    map2 Data
        (path [ "path", "to", "string", "value" ] (single string))
        (path [ "path", "to", "int", "values" ] (list int))


mytext = run dataDecoder
      """
      <root>
          <path>
              <to>
                  <string>
                      <value>SomeString</value>
                  </string>
                  <int>
                      <values>1</values>
                      <values>2</values>
                  </int>
              </to>
          </path>
      </root>
      """


main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

toText res =
    case res of
        Ok x -> .string x
        Err x -> "Something went wrong!"

view model =
  div []
    [ div [] [text (toText mytext)]]
