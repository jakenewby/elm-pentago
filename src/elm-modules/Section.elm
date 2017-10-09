module Section exposing (..)

import Cell
import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init sectionId =
    ( { sectionId = sectionId }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class ("section section-" ++ (toString (.sectionId model))) ]
        (map Cell (range 1 9))



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
