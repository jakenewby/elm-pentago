module Main exposing (..)

import Html exposing (Html, button, text, div)
import Html.Attributes exposing (class)


-- import Html.Events exposing (onClick)

import Maybe exposing (..)


cellsDom : List Cell -> List (Html msg)
cellsDom cells =
    List.map cellDom cells


cellDom : Cell -> Html msg
cellDom cell =
    let
        ( sectionId, cellId ) =
            (.coordinates cell)
    in
        div [ class (cellClass cell) ]
            [ div [ class "token token--player-1" ] []
            ]


cellClass : Cell -> String
cellClass cell =
    let
        ( sectionId, cellId ) =
            (.coordinates cell)
    in
        "cell cell-" ++ (toString cellId)


sectionsDom : List Section -> List (Html msg)
sectionsDom sections =
    [ (div [ class "board__row" ]
        (List.map
            sectionDom
            (List.take 2 sections)
        )
      )
    , (div
        [ class "board__row" ]
        (List.map
            sectionDom
            (List.drop 2 sections)
        )
      )
    ]


sectionDom : Section -> Html msg
sectionDom section =
    div [ class (sectionClass section) ] (cellsDom (.cells section))


sectionClass : Section -> String
sectionClass section =
    "section section-" ++ (toString (.id section))


initSections : List Section
initSections =
    List.map initSection (List.range 1 4)


initSection : Int -> Section
initSection id =
    { id = id, angle = 0, cells = (initSectionCells id) }


initSectionCells : Int -> List Cell
initSectionCells sectionId =
    List.map (initSectionCell sectionId) (List.range 1 9)


initSectionCell : Int -> Int -> Cell
initSectionCell sectionId cellId =
    { coordinates = ( sectionId, cellId )
    , token = Nothing
    }


updateAngle : Int -> Direction -> Int
updateAngle angle direction =
    case direction of
        Clockwise ->
            angle + 90

        CounterClockwise ->
            angle - 90



---- MODEL ----


type alias Model =
    { sections : List Section }


type alias Section =
    { id : Int
    , angle : Int
    , cells : List Cell
    }


type alias Cell =
    { coordinates : ( Int, Int )
    , token : Maybe Token
    }


type alias Token =
    { player : Player
    }


type alias Player =
    { id : Int
    , name : String
    }


type Direction
    = Clockwise
    | CounterClockwise


init : ( Model, Cmd Msg )
init =
    ( { sections = initSections }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | RotateSection Section Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RotateSection section direction ->
            let
                rotateSection : Section -> Section
                rotateSection s =
                    if (.id s) == (.id section) then
                        { section | angle = updateAngle (.angle section) direction }
                    else
                        section
            in
                ( { model | sections = List.map rotateSection (.sections model) }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "board" ] (sectionsDom (.sections model))



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
