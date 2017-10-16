module Main exposing (..)

import Html exposing (Html, button, text, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe exposing (..)


cellsDom : Model -> List Cell -> List (Html Msg)
cellsDom model cells =
    List.map (cellDom model) cells


cellDom : Model -> Cell -> Html Msg
cellDom model cell =
    let
        token =
            (.token cell)

        ( sectionId, cellId ) =
            (.coordinates cell)
    in
        case token of
            Just token ->
                div
                    [ class (cellClass cell)
                    ]
                    [ div
                        [ class ("token token--player-" ++ (toString (.playerId token)))
                        ]
                        []
                    ]

            _ ->
                div
                    [ class (cellClass cell)
                    , (onClick (AddToken cell))
                    ]
                    []


cellClass : Cell -> String
cellClass cell =
    let
        ( sectionId, cellId ) =
            (.coordinates cell)
    in
        "cell cell-" ++ (toString cellId)


sectionsDom : Model -> List (Html Msg)
sectionsDom model =
    let
        topRow =
            List.take 2 (.sections model)

        bottomRow =
            List.drop 2 (.sections model)
    in
        [ (div [ class "board__row" ]
            (List.concat
                [ (List.map
                    (sectionDom model)
                    topRow
                  )
                , (List.map
                    rotationControls
                    topRow
                  )
                ]
            )
          )
        , (div
            [ class "board__row" ]
            (List.concat
                [ (List.map
                    (sectionDom model)
                    bottomRow
                  )
                , (List.map
                    rotationControls
                    bottomRow
                  )
                ]
            )
          )
        ]


getCellsForSection : List Cell -> Section -> List Cell
getCellsForSection cells section =
    List.filter (\cell -> (getCellSectionId cell) == (.id section)) cells


getCellSectionId : Cell -> Int
getCellSectionId cell =
    let
        ( sectionId, _ ) =
            (.coordinates cell)
    in
        sectionId


rotationControls : Section -> Html Msg
rotationControls section =
    div [ class "rotation-control-group" ]
        [ button
            [ (class ("section-" ++ (toString (.id section)) ++ "__rotate-clockwise-btn"))
            , (onClick (RotateSection section Clockwise))
            ]
            []
        , button
            [ (class ("section-" ++ (toString (.id section)) ++ "__rotate-counterclockwise-btn"))
            , (onClick (RotateSection section CounterClockwise))
            ]
            []
        ]


sectionDom : Model -> Section -> Html Msg
sectionDom model section =
    let
        sectionCells =
            (getCellsForSection (.cells model) section)
    in
        div
            [ (class (sectionClass section)), (style [ ( "transform", "rotate(" ++ (toString (.angle section) ++ "deg)") ) ]) ]
            (cellsDom model sectionCells)


sectionClass : Section -> String
sectionClass section =
    "section section-" ++ (toString (.id section))


initSections : List Section
initSections =
    List.map initSection (List.range 1 4)


initSection : Int -> Section
initSection id =
    Section id 0


initSectionCells : Section -> List Cell
initSectionCells section =
    List.map (initSectionCell (.id section)) (List.range 1 9)


initSectionCell : Int -> Int -> Cell
initSectionCell sectionId cellId =
    Cell ( sectionId, cellId ) Nothing


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
    , cells : List Cell
    , currentPlayerId : Int
    , placedToken : Bool
    , winningPlayerId : Maybe Int
    }


type alias Section =
    { id : Int
    , angle : Int
    }


type alias Cell =
    { coordinates : ( Int, Int )
    , token : Maybe Token
    }


type alias Token =
    { playerId : Int
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
    let
        sections =
            initSections
    in
        ( { sections = sections
          , players = [ (Player 1 ""), (Player 2 "") ]
          , cells = List.concatMap (initSectionCells) sections
          , currentPlayerId = 1
          , placedToken = False
          , winningPlayerId = Nothing
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = NoOp
    | RotateSection Section Direction
    | AddToken Cell
    | ClearBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RotateSection section direction ->
            let
                newPlayerId =
                    case (.currentPlayerId model) of
                        1 ->
                            2

                        2 ->
                            1

                        _ ->
                            1
            in
                if (.placedToken model) then
                    ( { model
                        | sections =
                            (List.map
                                (\s ->
                                    if (.id s) == (.id section) then
                                        { section
                                            | angle = (updateAngle (.angle section) direction)
                                        }
                                    else
                                        s
                                )
                                (.sections model)
                            )
                        , currentPlayerId = newPlayerId
                        , placedToken = False
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        AddToken cell ->
            let
                setToken : Cell -> Cell -> Cell
                setToken c1 c2 =
                    if (.coordinates c1) == (.coordinates c2) then
                        Cell (.coordinates c2) (Just (Token (.currentPlayerId model)))
                    else
                        c2
            in
                if not (.placedToken model) then
                    ( { model
                        | cells = (List.map (setToken cell) (.cells model))
                        , placedToken = True
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        ClearBoard ->
            init



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "site" ]
        [ div [ class "board" ]
            (sectionsDom model)
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
