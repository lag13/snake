module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Char
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d


-- import AnimationFrame
-- Looks like I need to add the svg package to elm-package.json for these imports to work

import Svg
import Svg.Attributes as SvgAtt
import Keyboard


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { pos : Point2d.Point2d
    , dir : Direction2d.Direction2d
    }


init : ( Model, Cmd Msg )
init =
    ( { pos = Point2d.origin, dir = Direction2d.positiveY }, Cmd.none )


type Msg
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NoMovement
    | SecondPassed Time



-- TODO: Probably make a "Snake" type that this will return instead of
-- the entire model.


updateSnakePos : Model -> Direction2d.Direction2d -> Model
updateSnakePos model dir =
    { model | pos = Point2d.translateBy (Direction2d.toVector dir) model.pos, dir = dir }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveUp ->
            ( updateSnakePos model Direction2d.negativeY, Cmd.none )

        MoveDown ->
            ( updateSnakePos model Direction2d.positiveY, Cmd.none )

        MoveLeft ->
            ( updateSnakePos model Direction2d.negativeX, Cmd.none )

        MoveRight ->
            ( updateSnakePos model Direction2d.positiveX, Cmd.none )

        NoMovement ->
            ( model, Cmd.none )

        SecondPassed _ ->
            ( updateSnakePos model model.dir, Cmd.none )



-- Keyboard.downs re-fires when holding a specific key down. I wonder
-- if the OS refires the key if it is held down? Or maybe that is how
-- the keyboard itself behaves? Keboard.presses only fires for
-- printing keys as discussed here:
-- https://github.com/elm-lang/keyboard/issues/3. I think people often
-- use keydown to activate some functionality and keyup to stop it.
-- These are the key codes for the arrow keys.


upArrowKeyCode =
    38


downArrowKeyCode =
    40


leftArrowKeyCode =
    37


rightArrowKeyCode =
    39



-- I thought I'd be able to do specify previously defined variables on
-- the left side of -> in a case statement and it would basically do
-- an == check, but it turns out this is not possible:
-- https://github.com/elm-lang/elm-compiler/issues/1526. If you just
-- put a variable name then you just define a variable with the value
-- of the argument to the case statement. Best to use if/else instead
-- or map the keycode to a union type I suppose. I could just put the
-- raw numbers there but I kind of want the variable name to better
-- indicate what these numbers are.


key : Keyboard.KeyCode -> Msg
key keyCode =
    if keyCode == upArrowKeyCode then
        MoveUp
    else if keyCode == downArrowKeyCode then
        MoveDown
    else if keyCode == leftArrowKeyCode then
        MoveLeft
    else if keyCode == rightArrowKeyCode then
        MoveRight
    else
        NoMovement


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second SecondPassed
        , Keyboard.downs key
        ]



-- TODO: Research the "viewbox" attribute for SVG


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , Svg.svg [ SvgAtt.width "100px", SvgAtt.height "100px" ]
            [ snakeCell model.pos
            ]
        ]


snakeCell : Point2d.Point2d -> Svg.Svg Msg
snakeCell pos =
    Svg.rect
        [ SvgAtt.x (toString (Point2d.xCoordinate pos))
        , SvgAtt.y (toString (Point2d.yCoordinate pos))
        , SvgAtt.width "10"
        , SvgAtt.height "10"
        , SvgAtt.fill "red"
        , SvgAtt.stroke "black"
        , SvgAtt.strokeWidth "2"
        ]
        []
