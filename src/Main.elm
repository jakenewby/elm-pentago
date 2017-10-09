module Main exposing (..)

import Html exposing (Html, button, text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
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
            []


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
    div [ (class (sectionClass section)), (style [ ( "transform", "rotate(" ++ (toString (.angle section) ++ "deg)") ) ]) ]
        (cellsDom (.cells section))


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
    { sections : List Section
    , players : List Player
    }


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
    { player : Maybe Player
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
    ( { sections = initSections
      , players =
            List.map
                (\ind -> { id = ind, name = "" })
                (List.range 1 2)
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | RotateSection (Maybe Section) Direction



-- | AddToken Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RotateSection section direction ->
            case section of
                Nothing ->
                    ( model, Cmd.none )

                Just section ->
                    ( { model
                        | sections =
                            List.map
                                (\s ->
                                    if (.id s) == (.id section) then
                                        { section | angle = updateAngle (.angle section) direction }
                                    else
                                        s
                                )
                                (.sections model)
                      }
                    , Cmd.none
                    )



-- AddToken cell ->
--     ( { model
--         | sections =
--             (List.map
--                 (\section ->
--                     { section
--                         | cells =
--                             (List.map
--                                 (\c ->
--                                     let
--                                         player =
--                                             List.head (.players model)
--                                     in
--                                         case player of
--                                             Nothing ->
--                                                 c
--
--                                             Just player ->
--                                                 if (.coordinates c) == (.coordinates cell) then
--                                                     { c | token = { player = player } }
--                                                 else
--                                                     c
--                                 )
--                                 (.cells section)
--                             )
--                     }
--                 )
--                 (.sections model)
--             )
--       }
--     , Cmd.none
--     )
---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "site" ]
        [ div [ class "board" ]
            (sectionsDom (.sections model))
        , div
            [ class "controls" ]
            [ button
                [ onClick (RotateSection (List.head (.sections model)) Clockwise)
                ]
                [ text "rotate" ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
