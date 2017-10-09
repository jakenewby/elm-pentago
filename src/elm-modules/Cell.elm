module Cell exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)


---- MODEL ----


type alias Model =
    { cellId : Int }


init : Int -> ( Model, Cmd Msg )
init cellId =
    ( { cellId = cellId }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class ("cell cell-" ++ (toString (.cellId model))) ] []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
