module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe exposing (..)


cells : Model -> List Cell -> List (Html Msg)
cells model cells =
    List.map (cell model) cells


cell : Model -> Cell -> Html Msg
cell model cell =
    let
        token =
            .token cell

        ( sectionId, cellId ) =
            .coordinates cell
    in
    case token of
        Just token ->
            div
                [ class (cellClass cell)
                ]
                [ div
                    [ class ("token token--player-" ++ toString (.playerId token))
                    ]
                    []
                ]

        _ ->
            div
                [ class (cellClass cell)
                , onClick (AddToken cell)
                ]
                []


cellClass : Cell -> String
cellClass cell =
    let
        ( sectionId, cellId ) =
            cell.coordinates
    in
    "cell cell-" ++ toString cellId


sections : Model -> List (Html Msg)
sections model =
    let
        topRow =
            List.take 2 model.sections

        bottomRow =
            List.drop 2 model.sections
    in
    [ div [ class "board__row" ]
        (List.concat
            [ List.map
                (section model)
                topRow
            ]
        )
    , div
        [ class "board__row" ]
        (List.concat
            [ List.map
                (section model)
                bottomRow
            ]
        )
    ]


section : Model -> Section -> Html Msg
section model section =
    let
        sectionCells =
            getCellsForSection model.cells section
    in
    div
        [ class (sectionClass section), style [ ( "transform", "rotate(" ++ (toString section.angle ++ "deg)") ) ] ]
        (cells model sectionCells)


sectionClass : Section -> String
sectionClass section =
    "section section-" ++ toString section.id


getCellsForSection : List Cell -> Section -> List Cell
getCellsForSection cells section =
    List.filter (\cell -> getCellSectionId cell == section.id) cells


getCellSectionId : Cell -> Int
getCellSectionId cell =
    let
        ( sectionId, _ ) =
            cell.coordinates
    in
    sectionId


rotationControls : Section -> Html Msg
rotationControls section =
    div [ class ("rotation-control-group rotation-control-group--section-" ++ toString (.id section)) ]
        [ button
            [ class ("section-" ++ toString (.id section) ++ "__rotate-clockwise-btn")
            , onClick (RotateSection section Clockwise)
            ]
            [ text ("S" ++ toString section.id ++ "CW") ]
        , button
            [ class ("section-" ++ toString (.id section) ++ "__rotate-counterclockwise-btn")
            , onClick (RotateSection section CounterClockwise)
            ]
            [ text ("S" ++ toString section.id ++ "CCW") ]
        ]


initSections : List Section
initSections =
    List.map initSection (List.range 1 4)


initSection : Int -> Section
initSection id =
    Section id 0


initSectionCells : Section -> List Cell
initSectionCells section =
    List.map (initSectionCell section.id) (List.range 1 9)


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


initModel : ( Model, Cmd Msg )
initModel =
    let
        sections =
            initSections
    in
    ( { sections = sections
      , players = [ Player 1 "", Player 2 "" ]
      , cells = List.concatMap initSectionCells sections
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
                    case model.currentPlayerId of
                        1 ->
                            2

                        2 ->
                            1

                        _ ->
                            1
            in
            if model.placedToken then
                ( { model
                    | sections =
                        List.map
                            (\s ->
                                if s.id == section.id then
                                    { section
                                        | angle = updateAngle section.angle direction
                                    }
                                else
                                    s
                            )
                            model.sections
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
                    if c1.coordinates == c2.coordinates then
                        Cell c2.coordinates (Just (Token model.currentPlayerId))
                    else
                        c2
            in
            if not model.placedToken then
                ( { model
                    | cells = List.map (setToken cell) model.cells
                    , placedToken = True
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        ClearBoard ->
            initModel



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "site" ]
        [ div [ class "rotation-controls" ] (List.map rotationControls (List.filter (\s -> not (s.id % 2 == 0)) model.sections))
        , div [ class "board" ]
            (sections model)
        , div [ class "rotation-controls" ] (List.map rotationControls (List.filter (\s -> s.id % 2 == 0) model.sections))
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = initModel
        , update = update
        , subscriptions = always Sub.none
        }
