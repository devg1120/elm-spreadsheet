module TestTextArea exposing (main)

import Browser

import Html exposing (Html, p, ol, li, div, textarea, button, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as Attr


main =
    Browser.element
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init =  init
        }


type alias Model =
    List String


defaultModel : Model
defaultModel =
    [ "One", "Two", "Three" ]


init : {} -> ( Model, Cmd Msg )
init data =
    (defaultModel , Cmd.none)


type Msg
    = UpdateValue Int String
    | ShiftValue
    | Reset


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "To reproduce:" ]
        , ol []
            [ li [] [ text "Click 'Remove first textarea' once. Observe that the remaining textarea values are correct." ]
            , li [] [ text "Click 'Reset'." ]
            , li [] [ text "Enter the values '1', '2', and '3' into the three textareas." ]
            , li [] [ text "Click 'Remove first textarea' once. Observe that the remaining textarea values are incorrect." ]
            , li [] [ text "Inspect the DOM. Notice that in the DOM the correct textarea values are shown." ]
            ]
        , div []
            [ button [ onClick ShiftValue ] [ text "Remove first textarea" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div []
            (List.indexedMap viewTextarea model)
        ]


viewTextarea : Int -> String -> Html Msg
viewTextarea index value =
    textarea [ onInput <| UpdateValue index, Attr.value value ] []
    --textarea [ ] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateValue index value ->
            ((List.indexedMap (updateValue index value) model) , Cmd.none)
            --(model, Cmd.none)

        ShiftValue ->
            ((List.tail model |> Maybe.withDefault []) , Cmd.none)

        Reset ->
            (defaultModel , Cmd.none)


updateValue : Int -> String -> Int -> String -> String
updateValue targetIndex targetValue index value =
    if index == targetIndex then
        targetValue 
    else
        value 

