module App exposing (..)

import Array.Hamt as Array exposing (Array)
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (attribute, draggable, id, src, style)
import Reorderable.State exposing (Point, State, ViewableReorderable(..))
import Reorderable.Update


---- MODEL ----


type alias Model =
    { dragState : State String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { dragState = initDragState <| Array.fromList [ "a", "b", "c", "d" ]
      }
    , Cmd.none
    )


initDragState : Array String -> State String
initDragState items =
    { placeholder = Nothing
    , items = items
    , reorderedItems = items
    , animating = False
    }



---- UPDATE ----


type Msg
    = NoOp
    | DragMsg Reorderable.Update.DragMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragMsg dragMsg ->
            updateDrag dragMsg model

        NoOp ->
            ( model, Cmd.none )


updateDrag : Reorderable.Update.DragMsg -> Model -> ( Model, Cmd Msg )
updateDrag dragMsg model =
    case Reorderable.Update.update dragMsg model.dragState of
        Ok ( newDragState, cmd ) ->
            case dragMsg of
                Reorderable.Update.DragHold { placeholder, reorderableBounds } ->
                    ( { model | dragState = newDragState }, Cmd.none )

                _ ->
                    ( { model | dragState = newDragState }, Cmd.map DragMsg cmd )

        Err _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewItems model.dragState.reorderedItems
        , viewPlaceholder model.dragState.placeholder
        ]


toPx : number -> String
toPx num =
    toString num ++ "px"


viewPlaceholder : Maybe (Reorderable.State.Placeholder String) -> Html Msg
viewPlaceholder maybePlaceholder =
    case maybePlaceholder of
        Just placeholder ->
            span
                [ id "reorderable-placeholder"
                , style
                    [ ( "position", "fixed" )
                    , ( "left", toPx placeholder.point.x )
                    , ( "top", toPx placeholder.point.y )
                    , ( "margin", "0" )
                    , ( "padding", "2em" )
                    , ( "border", "solid 3px fuchsia" )
                    ]
                ]
                [ Html.text placeholder.draggable
                ]

        Nothing ->
            Html.text ""


viewItems : Array String -> Html Msg
viewItems items =
    items
        |> Array.indexedMap viewItem
        |> Array.toList
        |> div
            [ style
                [ ( "display", "flex" )
                , ( "width", "100%" )
                ]
            ]


viewItem : Int -> String -> Html Msg
viewItem index text =
    span
        [ style
            [ ( "padding", "2em" )
            , ( "border", "solid 3px fuchsia" )
            ]
        , draggable "false"
        , attribute "data-reorderable-index" (toString index)
        ]
        [ Html.text text
        ]



---- PROGRAM ----


dragConfig : Reorderable.State.Config
dragConfig =
    { animate = True }


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.map DragMsg <|
                    Reorderable.Update.subscriptions dragConfig model.dragState
        }
