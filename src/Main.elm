module Main exposing (Cell, Direction(..), Model, Msg(..), Player, Quadrant, Token, XYCoordinatePair, cellClass, classForQuadrant, getWinner, initCell, initCells, initModel, initQuadrant, initQuadrants, main, rotateCells, rotationControls, update, updateAngle, view, viewCell, viewCells, viewQuadrant, viewQuadrants)

import Browser exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe exposing (..)



---- MODEL ----


initQuadrants : List Quadrant
initQuadrants =
    List.map initQuadrant (List.range 1 4)


initQuadrant : Int -> Quadrant
initQuadrant id =
    Quadrant id 0 initCells


initCells : List Cell
initCells =
    let
        cellRange =
            List.range 1 3
    in
    List.concatMap
        (\x ->
            List.map
                (\y ->
                    initCell ( x, y )
                )
                cellRange
        )
        cellRange


initCell : XYCoordinatePair -> Cell
initCell coordinates =
    Cell coordinates Nothing


type alias Model =
    { quadrants : List Quadrant
    , players : List Player
    , didPlaceToken : Bool
    , currentPlayerId : Int
    , winningPlayerId : Maybe Int
    }


type alias Quadrant =
    { id : Int
    , angle : Int
    , cells : List Cell
    }


type alias Cell =
    { coordinates : XYCoordinatePair
    , token : Maybe Token
    }


type alias XYCoordinatePair =
    ( Int, Int )


type alias Token =
    { playerId : Int }


type alias Player =
    { id : Int
    , name : String
    }


type Direction
    = Clockwise
    | CounterClockwise


initModel : Model
initModel =
    let
        quadrants =
            initQuadrants
    in
    { quadrants = quadrants
    , players = [ Player 1 "", Player 2 "" ]
    , currentPlayerId = 1
    , didPlaceToken = False
    , winningPlayerId = Nothing
    }



---- UPDATE ----


getWinner : Model -> Maybe Player
getWinner model =
    let
        quadrants : List Quadrant
        quadrants =
            model.quadrants

        cells : List Cell
        cells =
            List.concatMap (\q -> q.cells) quadrants

        numTokensPlaced : Int
        numTokensPlaced =
            List.filter (\c -> c.token /= Nothing) cells
                |> List.length
    in
    -- not possible to win in less than 9 moves
    if numTokensPlaced < 9 then
        Nothing

    else
        -- find the winner
        Nothing


rotateCells : List Cell -> Direction -> List Cell
rotateCells cells direction =
    case direction of
        Clockwise ->
            List.map
                (\c ->
                    case c.coordinates of
                        ( 1, y ) ->
                            { c | coordinates = ( y, 3 ) }

                        ( 2, y ) ->
                            { c | coordinates = ( y, 2 ) }

                        ( 3, y ) ->
                            { c | coordinates = ( y, 1 ) }

                        _ ->
                            c
                )
                cells

        CounterClockwise ->
            List.map
                (\c ->
                    case c.coordinates of
                        ( x, 1 ) ->
                            { c | coordinates = ( 3, x ) }

                        ( x, 2 ) ->
                            { c | coordinates = ( 2, x ) }

                        ( x, 3 ) ->
                            { c | coordinates = ( 1, x ) }

                        _ ->
                            c
                )
                cells


updateAngle : Int -> Direction -> Int
updateAngle angle direction =
    case direction of
        Clockwise ->
            angle + 90

        CounterClockwise ->
            angle - 90


type Msg
    = NoOp
    | RotateQuadrant Quadrant Direction
    | AddToken Quadrant Cell
    | ClearBoard


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        RotateQuadrant quadrant direction ->
            let
                newPlayerId : Int
                newPlayerId =
                    case model.currentPlayerId of
                        1 ->
                            2

                        2 ->
                            1

                        _ ->
                            1

                newQuadrants : List Quadrant
                newQuadrants =
                    List.map
                        (\q ->
                            if q.id == quadrant.id then
                                { quadrant
                                    | angle = updateAngle quadrant.angle direction
                                    , cells = rotateCells quadrant.cells direction
                                }

                            else
                                q
                        )
                        model.quadrants

                didCurrentPlayerWin : Bool
                didCurrentPlayerWin =
                    let
                        winner : Maybe Player
                        winner =
                            getWinner model
                    in
                    case winner of
                        Just player ->
                            player.id == model.currentPlayerId

                        Nothing ->
                            False
            in
            if model.didPlaceToken then
                { model
                    | quadrants = newQuadrants
                    , currentPlayerId = newPlayerId
                    , didPlaceToken = False
                }

            else
                model

        AddToken quadrant cell ->
            let
                setToken : Cell -> Cell -> Cell
                setToken c1 c2 =
                    if c1.coordinates == c2.coordinates then
                        Cell c2.coordinates (Just (Token model.currentPlayerId))

                    else
                        c2

                didCurrentPlayerWin : Bool
                didCurrentPlayerWin =
                    let
                        winner : Maybe Player
                        winner =
                            getWinner model
                    in
                    case winner of
                        Just player ->
                            player.id == model.currentPlayerId

                        Nothing ->
                            False

                newCells : List Cell
                newCells =
                    List.map (setToken cell) quadrant.cells

                newQuadrants : List Quadrant
                newQuadrants =
                    List.map
                        (\q ->
                            if q.id == quadrant.id then
                                { quadrant | cells = newCells }

                            else
                                q
                        )
                        model.quadrants
            in
            if not model.didPlaceToken then
                if didCurrentPlayerWin then
                    { model
                        | quadrants = newQuadrants
                        , didPlaceToken = True
                        , winningPlayerId = Just model.currentPlayerId
                    }

                else
                    { model
                        | quadrants = newQuadrants
                        , didPlaceToken = True
                    }

            else
                model

        ClearBoard ->
            initModel



---- VIEW ----


viewCells : Quadrant -> List (Html Msg)
viewCells quadrant =
    List.map (viewCell quadrant) quadrant.cells


viewCell : Quadrant -> Cell -> Html Msg
viewCell quadrant cell =
    case cell.token of
        Just token ->
            div
                [ class (cellClass cell)
                ]
                [ div
                    [ class ("token token--player-" ++ String.fromInt (.playerId token))
                    ]
                    []
                ]

        _ ->
            div
                [ class (cellClass cell)
                , onClick (AddToken quadrant cell)
                ]
                []


cellClass : Cell -> String
cellClass cell =
    let
        ( quadrantId, cellId ) =
            cell.coordinates
    in
    "cell cell-" ++ String.fromInt cellId


viewQuadrants : Model -> List (Html Msg)
viewQuadrants model =
    let
        topRowQuadrants : List Quadrant
        topRowQuadrants =
            List.take 2 model.quadrants

        bottomRowQuadrants : List Quadrant
        bottomRowQuadrants =
            List.drop 2 model.quadrants
    in
    [ div [ class "board__row" ]
        (List.concat
            [ List.map
                (viewQuadrant model)
                topRowQuadrants
            ]
        )
    , div
        [ class "board__row" ]
        (List.concat
            [ List.map
                (viewQuadrant model)
                bottomRowQuadrants
            ]
        )
    ]


viewQuadrant : Model -> Quadrant -> Html Msg
viewQuadrant model quadrant =
    let
        quadrantRotation : String
        quadrantRotation =
            "rotate(" ++ String.fromInt quadrant.angle ++ "deg)"
    in
    div
        [ class ("quadrant " ++ classForQuadrant quadrant), style "transform" quadrantRotation ]
        (viewCells quadrant)


classForQuadrant : Quadrant -> String
classForQuadrant quadrant =
    "quadrant-" ++ String.fromInt quadrant.id


rotationControls : Quadrant -> Html Msg
rotationControls quadrant =
    let
        quadrantClass =
            classForQuadrant quadrant

        quadrantIdString =
            String.fromInt quadrant.id
    in
    div [ class ("rotation-control-group rotation-control-group--" ++ quadrantClass) ]
        [ button
            [ class (quadrantClass ++ "__rotate-clockwise-btn")
            , onClick (RotateQuadrant quadrant Clockwise)
            ]
            [ text ("S" ++ quadrantIdString ++ "CW") ]
        , button
            [ class (quadrantClass ++ "__rotate-counterclockwise-btn")
            , onClick (RotateQuadrant quadrant CounterClockwise)
            ]
            [ text ("S" ++ quadrantIdString ++ "CCW") ]
        ]


view : Model -> Html Msg
view model =
    div [ class "site" ]
        [ div [ class "rotation-controls" ] (List.map rotationControls (List.filter (\q -> not (modBy 2 q.id == 0)) model.quadrants))
        , div [ class "board" ]
            (viewQuadrants model)
        , div [ class "rotation-controls" ] (List.map rotationControls (List.filter (\q -> modBy 2 q.id == 0) model.quadrants))
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
