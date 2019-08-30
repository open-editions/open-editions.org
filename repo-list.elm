-- Press a button to send a GET request for random cat GIFs.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder, field, string, map)
import List as L exposing (map)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type Model
  = Failure
  | Loading
  | Success (List Repo)


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, fetchRepos)



-- UPDATE


type Msg
  = MorePlease
  | GotRepos (Result Http.Error (List Repo))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, fetchRepos)

    GotRepos result ->
      case result of
        Ok items ->
          (Success items, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Cats" ]
    , viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random cat for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success repos ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , ul [] (L.map viewRepo repos)
        ]

viewRepo : Repo -> Html Msg
viewRepo repo = li [] [text (repo.name)]

-- HTTP

type alias Repo = { name : String, open_issues : Int }


fetchRepos : Cmd Msg
fetchRepos =
  Http.get
    { url = "https://api.github.com/orgs/open-editions/repos"
    , expect = Http.expectJson GotRepos repoListDecoder
    }

repoDecoder : Decoder Repo
repoDecoder = D.map2
                Repo
                (D.field "name" D.string)
                (D.field "open_issues" D.int)

repoListDecoder : Decoder (List Repo)
repoListDecoder = D.list repoDecoder

