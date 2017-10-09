module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (..)


cellsDom : List (Html msg)
cellsDom =
    List.map cellDom (List.range 1 9)


cellDom : Int -> Html msg
cellDom id =
    div [ class (cellClass id) ]
        [ div [ class "token token--player-1" ] []
        ]


cellClass : Int -> String
cellClass id =
    "cell cell-" ++ (toString id)


sectionsDom : List (Html msg)
sectionsDom =
    List.map sectionDom (List.range 1 4)


sectionDom : Int -> Html msg
sectionDom id =
    div [ class (sectionClass id) ] cellsDom


sectionClass : Int -> String
sectionClass id =
    "section section-" ++ (toString id)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "board" ] sectionsDom



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
