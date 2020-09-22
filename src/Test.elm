--module AutoExpand exposing (main)
module Test exposing (main)
-- https://embed.ellie-app.com/Gnv9Bznh4na1/0

import Browser
import Html exposing (Html, div, p, br, textarea, text)
import Html.Attributes exposing (rows, style)
import Html.Events exposing (onInput, on)
import Json.Decode exposing (Decoder, field, at, map2, int, string)


type alias Model =
    { rows : Int
    , inputText : String
    }


type alias Config =
    { padding : Float
    , lineHeight : Float
    , minRows : Int
    , maxRows : Int
    }


config : Config
config =
    { padding = 10
    , lineHeight = 20
    , minRows = 1
    , maxRows = 4
    }


initModel : Model
initModel =
    { rows = config.minRows
    , inputText = ""
    }


type Msg
    = NewValues String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewValues inputText height ->
            ( { model
                | inputText = inputText
                , rows = getRows height
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div  containerStyle 
        [ div
            --[ style [ ( "width", "200px" ) ]
            [ style  "width" "200px"  ]
            [ textarea
                
                (
                List.append 
                [ on "input" inputDecoder
                , rows model.rows
                , Html.Attributes.value model.inputText
                ]  (textareaStyles model.rows)
                )
                
                
                []

            , p []
                [ text ("Rows: " ++ String.fromInt model.rows)
                , br [] []
                , text ("Min: " ++ String.fromInt config.minRows)
                , br [] []
                , text ("Max: " ++ String.fromInt config.maxRows)
                ]
            ]
        ]


getRows : Int -> Int
getRows scrollHeight =
    ((toFloat scrollHeight - 2 * config.padding) / config.lineHeight)
        |> ceiling
        |> clamp config.minRows config.maxRows


inputDecoder : Decoder Msg
inputDecoder =
    map2 NewValues
        (at [ "target", "value" ] string)
        (at [ "target", "scrollHeight" ] int)


containerStyle : List (Html.Attribute msg)
containerStyle =
        [ style "background-color" "rebeccapurple" 
        , style "color" "white" 
        , style "font-family" "sans-serif" 
        , style "width" "100vw" 
        , style "height" "100vh" 
        , style "display" "flex" 
        , style "align-items" "center" 
        , style "justify-content" "center" 
        , style "flex-direction" "column" 
        ]


textareaStyles : Int -> List (Html.Attribute msg)
textareaStyles rowCount =
        let 
          attr =
            if rowCount <= config.maxRows then
                      "visible"
            else
                    "scroll-y"
        in
        [ style  "padding" (String.fromFloat config.padding ++ "px" )
        , style  "border" "0 none" 
        , style  "border-radius" "2px" 
        , style  "box-sizing" "border-box" 
        , style  "line-height" (String.fromFloat config.lineHeight ++ "px" )
        , style  "width" "100%" 
        , style  "overflow" attr
        ]

init : {} -> ( Model, Cmd Msg)
init data =
         ( initModel, Cmd.none )


main =
    Browser.element
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        --, init = ( initModel, Cmd.none )
        , init =  init
        }
