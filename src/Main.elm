port module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Browser.Dom
import Browser.Events
import Csv
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy3, lazy4)
import Json.Decode as JD
import Json.Encode as JE
import Task
import Array2D
import Array2dOp


{--
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
--}


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias CellRef =
    { row : Int
    , col : Int
    }


type Cell
    = FloatCell Float
    | StringCell String

type alias AttrCell
    = {
         bg_color : String
        ,fg_color : String
       }

type alias AttrCol
    = {
        width : Float
       }

type alias AttrRow
    = {
        height : Float
       }

cellToString : Cell -> String
cellToString cell =
    case cell of
        FloatCell float ->
            String.fromFloat float

        StringCell str ->
            str


type alias Cells = Array (Array Cell)
type alias AttrCells = Array (Array AttrCell)

type alias Table =
    {
      cells: Cells
     ,attrs: AttrCells
     ,attr_cols:  Array AttrCol
     ,attr_rows:  Array AttrRow
    }

getTableSize : Table -> CellRef
getTableSize table =
    case Array.get 0 table.cells of
        Just firstRow ->
            CellRef (Array.length table.cells) (Array.length firstRow)

        Nothing ->
            CellRef (Array.length table.cells) 0


toCsv : Table -> String
toCsv table =
    let
        escapeDQ : Cell -> String
        escapeDQ cell =
            if String.contains "\"" <| cellToString cell then
                "\"" ++ (String.replace "\"" "\"\"" <| cellToString cell) ++ "\""

            else
                cellToString cell

        listTable =
            Array.toList <| Array.map (Array.toList << Array.map escapeDQ) table.cells

        -- _ = Debug.log "toCsv: " listTable
    in
    String.join "\n" <| List.map (String.join ",") listTable


fromCsv : String -> Cells
fromCsv csvStr =
    let
        { headers, records } =
            Csv.parse csvStr

        stringToCell : String -> Cell
        stringToCell str =
            case String.toFloat str of
                Just float ->
                    FloatCell float

                Nothing ->
                    StringCell str
    
    in
       Array.fromList <|
           [ Array.fromList <| List.map stringToCell headers ]
               ++ List.map (Array.fromList << List.map stringToCell) records
               

type MoveOver
    = UpOver
    | DownOver
    | LeftOver
    | RightOver
    | NoneOver


type Select
    = Cell CellRef 
    | Csv


type alias InputState =
    { selected : Select
    , isEditing : Bool
    , isAltDown : Bool
    , text : String
    }

type alias ViewRange =
     { colstart : Int
     , colend : Int
     , rowstart : Int
     , rowend : Int
     }

type alias Model =
    { table : Table
    , input : InputState
    , viewrange : ViewRange
    }

data_list =
  [  
    [ StringCell "A", StringCell "B" , StringCell "C",StringCell "D", StringCell "E"] 
    ,[StringCell "Name", FloatCell  2 , StringCell "Gusa Syou",StringCell "a", FloatCell 1] 
    ,[ StringCell "Bob",  FloatCell 3 ,     FloatCell 99,StringCell "b", FloatCell 2]
    ,[ StringCell "Sob",  FloatCell 4 ,     FloatCell 0.189,StringCell "c", FloatCell 3]
    ,[ StringCell "Aob",  FloatCell 5 ,     FloatCell 100002,StringCell "d", FloatCell 4]
    ,[ StringCell "gusa", FloatCell 6 ,      FloatCell 100002,StringCell "e", FloatCell 5]
    ,[ StringCell "関西", FloatCell 7 ,      FloatCell 100002,StringCell "f", FloatCell 6]
    ,[ StringCell "システム", FloatCell 8 ,      FloatCell 100002,StringCell "g", FloatCell 7]
    ,[ StringCell "LAST", FloatCell 9 ,      FloatCell 100002,StringCell "h", FloatCell 8]
  ]

attr_list2 =
  [  [ { bg_color = "#FFFFFF" }, { bg_color = "#FFFFFF" }, { bg_color = "#FFFFFF" }] 
    ,[ { bg_color = "#FFFFFF" }, { bg_color = "#FFFFFF" }, { bg_color = "#FFFFFF" }] 
    ,[ { bg_color = "#33F44F" }, { bg_color = "#FFFFFF" }, { bg_color = "#FFFFFF" }] 
    ,[ { bg_color = "#FFFFFF" }, { bg_color = "#FFCCFF" }, { bg_color = "#FFFFFF" }] 
    ,[ { bg_color = "#FFFFFF" }, { bg_color = "#FFCCFF" }, { bg_color = "#FFFFFF" }] 
  ]

attr_list =
  [  [ 
         { 
           bg_color = "#FFFFFF" 
          ,fg_color = "#0080ff" 
         } 
       , { 
           bg_color = "#FFFFFF" 
          ,fg_color = "#CC0000" 
         } 
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
     ] 
    ,[ 
         { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
          }
     ] 
    ,[ 
         { 
           bg_color = "#33F44F"
          ,fg_color = "#CC0000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
     ] 
    ,[ 
         { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFCCFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
     ] 
    ,[ 
         { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFCCFF"
          ,fg_color = "#000000" 
         }
       , { 
           bg_color = "#FFFFFF"
          ,fg_color = "#000000" 
         }
     ] 
  ]

init : JE.Value -> ( Model, Cmd Msg )
init data =
    let
        defaultCell : Cell
        defaultCell =  StringCell ""

        array2d = Array2D.fromList  data_list
        array2d2 = Array2dOp.array2Dexpand 20 20 defaultCell array2d
        initTable =
            { 
            --  cells = Array.fromList <| List.map Array.fromList data_list
              cells = array2d2.data
             ,attrs = Array.fromList <| List.map Array.fromList attr_list
             ,attr_cols =  Array.fromList  [ {width = 100.0} , {width = 100.0} ,{width = 200.0} ,{width = 100.0} ]
             ,attr_rows =  Array.fromList  [ {height = 30.0}, {height = 20.0},{height = 10.0},{height = 40.0}]
            }

        initInput =
            InputState (Cell <| CellRef 0 0) False False ""

--        initViewRange = {
--                      colstart = 1
--                    , colend   = 5
--                    , rowstart = 1
--                    , rowend   = 8
--                    }
        initViewRange = {
                      colstart = 2
                    , colend   = 4
                    , rowstart = 2
                    , rowend   = 7
                    }
    in
--    case JD.decodeValue (JD.array (JD.array jsonToCell)) data of
--        Ok table ->
--            let
--                table_ = { cells = table.cells 
--                          ,cols  = table.cols
--                          ,rows  = table.rows
--                         }
--            in
--            ( Model table_ initInput, Cmd.none )
--
--        Err _ ->
--            ( Model initTable initInput, Cmd.none )
    ( Model initTable initInput initViewRange , Cmd.none )


jsonToCell : JD.Decoder Cell
jsonToCell =
    JD.field "type" JD.string
        |> JD.andThen toCell


toCell : String -> JD.Decoder Cell
toCell typeStr =
    case typeStr of
        "Float" ->
            JD.map FloatCell (JD.field "Float" JD.float)

        "String" ->
            JD.map StringCell (JD.field "String" JD.string)

        _ ->
            JD.fail "operation JSON to Cell failed"



-- PORT


port saveTable : JE.Value -> Cmd msg


tableToJson : Table -> JE.Value
tableToJson table =
    JE.array (JE.array cellToJson) table.cells


cellToJson : Cell -> JE.Value
cellToJson cell =
    case cell of
        FloatCell float ->
            JE.object [ ( "type", JE.string "Float" ), ( "Float", JE.float float ) ]

        StringCell str ->
            JE.object [ ( "type", JE.string "String" ), ( "String", JE.string str ) ]



-- UPDATE


type Move
    = Up
    | Down
    | Left
    | Right

type CellEdit
    = AltDown
    | AltUp
    | EnterDown

moveCellRef : Move ->  (Int, Int) -> (Int, Int) -> CellRef -> Table -> (CellRef, MoveOver)
moveCellRef move (colstart, colend) (rowstart, rowend) cellRef table =
    let _ = Debug.log "cellRef:" cellRef in
    let
        { row, col } =
            cellRef

        --size =
        --    getTableSize table
    in
    case move of
        Up ->
            --{ cellRef | row = max 0 (row - 1) }
            --<SCOPE>--
            let
              newcellRef ={ cellRef | row = min 0  (row + 1) }
              over = if cellRef.row == newcellRef.row then
                        let _ = Debug.log "Up over:" row in
                        UpOver
                    else
                        let _ = Debug.log "" 0 in
                        NoneOver


            in
            (newcellRef , over)

        Down ->
            --{ cellRef | row = min (size.row - 1) (row + 1) }
            --{ cellRef | row = min 5 (row + 1) }
            --<SCOPE>--
            let
              -- newcellRef ={ cellRef | row = min 5  (row + 1) }
              newcellRef ={ cellRef | row = min (rowend - rowstart)  (row + 1) }
              over = if cellRef.row == newcellRef.row then
                        let _ = Debug.log "Down over:" row in
                        DownOver
                    else
                        let _ = Debug.log "" 0 in
                        NoneOver


            in
            (newcellRef , over)

        Left ->
            let
              newcellRef = { cellRef | col = max 0 (col - 1) }
              over = if cellRef.col == newcellRef.col then
                        let _ = Debug.log "Left over:" col in
                        LeftOver
                    else
                        let _ = Debug.log "" 0 in
                        NoneOver
            in
            (newcellRef , over)

        Right ->
            --{ cellRef | col = min (size.col - 1) (col + 1) }
            --{ cellRef | col = min 2 (col + 1) }
            --<SCOPE>--
            let
              --newcellRef ={ cellRef | col = min 2  (col + 1) }
              newcellRef ={ cellRef | col = min (colend - colstart)  (col + 1) }
              over = if cellRef.col == newcellRef.col then
                        let _ = Debug.log "Right over:" col in
                        RightOver
                    else
                        let _ = Debug.log "" 0 in
                        NoneOver


            in
            (newcellRef , over)


type Msg
    = Select Select
    | Move Move
    | StartEdit (Maybe CellRef) String
    | CancelEdit
    | Edit String
    | EditCsv String
    | Set
    | AddRow Int
    | AddCol Int
    | DeleteRow Int
    | DeleteCol Int
    | SaveToJs
--    | CellEdit CellEdit
    | TextAreaInput Int
    | None


focusCellEditor : Cmd Msg
focusCellEditor =
    Task.attempt (\_ -> None) (Browser.Dom.focus "cell-input")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --let _ = Debug.log "update..." msg in
    let
       colstart = model.viewrange.colstart
       colend  = model.viewrange.colend
       rowstart = model.viewrange.rowstart
       rowend  = model.viewrange.rowend
    in

    case msg of
        TextAreaInput code ->
            --let _ = Debug.log "TextAreaInput:" code in
            ( model, Cmd.none )
                
        Select select ->
            case select of
                Cell cellRef ->
                    ( { model | input = InputState (Cell cellRef) False False "" }, Cmd.none )

                Csv ->
                    ( { model | input = InputState Csv True False "" }, Cmd.none )

        Move move ->

            case model.input.selected of
                Cell cellRef ->
                    let
                        size =
                            getTableSize model.table

                        oldInput =
                            model.input
                        oldViewRange =
                            model.viewrange

--                        newInput =
--                            { oldInput | selected = Cell <| moveCellRef move (colstart, colend) (rowstart, rowend) cellRef model.table }

                        (cellref, moveover) = moveCellRef move (colstart, colend) (rowstart, rowend) cellRef model.table 
                        _ = Debug.log "moveover:" moveover 
                        newInput =
                            { oldInput | selected = Cell <| cellref }

                        newViewRenge =
                            case moveover of
                                UpOver ->
                                   let
                                      tmp = { oldViewRange | rowstart = max (oldViewRange.rowstart - 1) 1 }
                                   in
                                   if tmp.rowstart == oldViewRange.rowstart then
                                      { tmp | rowend   = oldViewRange.rowend     }
                                   else
                                      { tmp | rowend   = oldViewRange.rowend   - 1  }

                                DownOver ->
                                   let
                                      tmp = { oldViewRange | rowstart = oldViewRange.rowstart + 1   }
                                   in
                                   { tmp | rowend   = oldViewRange.rowend   + 1 }

                                LeftOver ->
                                   let
                                      tmp = { oldViewRange | colstart = max (oldViewRange.colstart - 1) 1 }
                                   in
                                   if tmp.rowstart == oldViewRange.rowstart then
                                      { tmp | colend   = oldViewRange.colend    }
                                   else
                                      { tmp | colend   = oldViewRange.colend   - 1 }

                                RightOver ->
                                   let
                                      tmp = { oldViewRange | colstart = oldViewRange.colstart + 1 }
                                   in
                                   { tmp | colend   = oldViewRange.colend   + 1 }

                                NoneOver ->
                                  oldViewRange

                    in
                    let
                       tmp = { model | input  = newInput}
                    in
                    ( { tmp | viewrange = newViewRenge}, Cmd.none )

                Csv ->
                    ( model, Cmd.none )

        StartEdit (Just cellRef) text ->
            ( { model | input = InputState (Cell cellRef) True False text }, focusCellEditor )

        StartEdit Nothing text ->
            ( { model | input = InputState model.input.selected True False text }, focusCellEditor )

        CancelEdit ->
            let
                prevInput =
                    model.input

                newInput =
                    { prevInput | isEditing = False, text = "" }
            in
            ( { model | input = newInput }, Cmd.none )

        Edit text ->
            let
                prevInput =
                    model.input

                newInput =
                    { prevInput | text = text }
            in
            ( { model | input = newInput }, Cmd.none )

        EditCsv csvStr ->
            let
               newTable = model.table 
            in
            update SaveToJs
                -- { model | table = fromCsv csvStr }
                { model | table = { newTable | cells = (fromCsv csvStr)} }

        Set ->
            case model.input.selected of
                Cell cellRef ->
                    case String.toFloat model.input.text of
                        Just num ->
                            update SaveToJs
                                { model
                                    | table = updateData cellRef (colstart, rowstart) (FloatCell num) model.table
                                    , input = InputState (Cell cellRef) False False ""
                                }

                        Nothing ->
                            update SaveToJs
                                { model
                                    | table = updateData cellRef (colstart, rowstart) (StringCell model.input.text) model.table
                                    , input = InputState (Cell cellRef) False False ""
                                }

                Csv ->
                    ( model, Cmd.none )

        AddRow rowIndex ->
            let
                ( first, second ) =
                    -- Array.splitAt rowIndex model.table
                    Array.splitAt rowIndex model.table.cells

                { col } =
                    getTableSize model.table

                -- newTable =
                --    Array.append (Array.push (Array.repeat col <| StringCell "") first) second

                newCells =
                    Array.append (Array.push (Array.repeat col <| StringCell "") first) second

                newTable = model.table
            in
            update SaveToJs
                -- { model | table = newTable }
                { model | table = {newTable | cells = newCells }}

        AddCol colIndex ->
            let
                mapFunc : Array Cell -> Array Cell
                mapFunc row =
                    Array.append (Array.push (StringCell "") <| Array.sliceUntil colIndex row) (Array.sliceFrom colIndex row)
                newTable = model.table
            in
            update SaveToJs
                -- { model | table = Array.map mapFunc model.table }
                { model | table = {newTable | cells = Array.map mapFunc model.table.cells }}

        DeleteRow rowIndex ->
            let
                newTable = model.table
            in
            update SaveToJs
                -- { model | table = Array.removeAt rowIndex model.table }
                { model | table = {newTable | cells = Array.removeAt rowIndex model.table.cells }}

        DeleteCol colIndex ->
            let
                newTable = model.table
            in
            update SaveToJs
                -- { model | table = Array.map (Array.removeAt colIndex) model.table }
                { model | table = {newTable | cells = Array.map (Array.removeAt colIndex) model.table.cells }}

        SaveToJs ->
            let
                newTable = model.table
            in
            ( model, saveTable <| tableToJson <| model.table )

{--
        CellEdit celledit ->
            -- let _ = Debug.log "CellEdit:"  celledit in
            if model.input.isEditing then
               case celledit of
                   AltUp ->
                          let new_input = model.input
                          in
                          ( { model | input = { new_input | isAltDown = False }}, Cmd.none )
                   AltDown ->
                          let new_input = model.input
                          in
                          ( { model | input = { new_input | isAltDown = True }}, Cmd.none )
                   EnterDown ->
                          if model.input.isAltDown then
                            --let _ = Debug.log "CellEdit: Alt : "  celledit in
                             ( model, Cmd.none )
                          else
                            --let _ = Debug.log "CellEdit:     : "  celledit in
                             ( model, Cmd.none )
            else
                   ( model, Cmd.none )
--}
        None ->
            ( model, Cmd.none )

updateData : CellRef -> (Int, Int) -> Cell -> Table -> Table
updateData { row, col } (colstart, rowstart) data table =
    let 
       --col_ = col + 1
       --row_ = row + 1
       col_ = col + (colstart-1)
       row_ = row + (rowstart-1)
    in
    --case Array.get row table.cells of
    --<SCOPE>--
    case Array.get row_ table.cells of
        Just rowArray ->
            let
              -- cells = Array.set row (Array.set col data rowArray) table.cells
              --<SCOPE>--
              cells = Array.set row_ (Array.set col_ data rowArray) table.cells
            in
              { table | cells = cells } 

        Nothing ->
            table



-- SUBSCRIPTIONS


keyDecoderDown : JD.Decoder Msg
keyDecoderDown =
    JD.map toDirection (JD.field "key" JD.string)


toDirection : String -> Msg
toDirection string =
    case String.length string of
        1 ->
            StartEdit Nothing string

        _ ->
            case string of
                "ArrowLeft" ->
                    Move Left

                "ArrowUp" ->
                    Move Up

                "ArrowRight" ->
                    Move Right

                "ArrowDown" ->
                    Move Down

                "Escape" ->
                    CancelEdit

                "Enter" ->
                    StartEdit Nothing ""

                _ ->
                    None

{--
-----------------------------------
inputEdittingKeyDecoderDown : JD.Decoder Msg
inputEdittingKeyDecoderDown =
    JD.map toEdittingDownKey (JD.field "key" JD.string)

toEdittingDownKey : String -> Msg
toEdittingDownKey string =
    let _ = Debug.log "Editting input Down:" string 
    in
            case string of
                "Alt" ->
                    CellEdit AltDown
                "Enter" ->
                    CellEdit EnterDown
                _ ->
                     None

inputEdittingKeyDecoderUp : JD.Decoder Msg
inputEdittingKeyDecoderUp =
    JD.map toEdittingUpKey (JD.field "key" JD.string)

toEdittingUpKey : String -> Msg
toEdittingUpKey string =
    --let _ = Debug.log "Editting input Up:" string 
    --in
            case string of
                "Alt" ->
                    CellEdit AltUp
                _ ->
                    None

-----------------------------------
--}

subscriptions : Model -> Sub Msg
subscriptions { input } =
    --let _ = Debug.log "input:" input in
    if input.isEditing then
        Sub.none
        {--
        Sub.batch
        [
          Browser.Events.onKeyDown inputEdittingKeyDecoderDown
        , Browser.Events.onKeyUp   inputEdittingKeyDecoderUp
        ]
        --}

    else
        Browser.Events.onKeyDown keyDecoderDown



-- VIEW



view : Model -> Html Msg
view model =
   div [][
    --viewCtlPanel  (getTableSize model.table) model.table.attr_cols model.input
    --viewCtlPanel   model.table.attr_cols model.input
    viewCtlPanel  model.input
    ,
    div [ A.class "wrap" 
         ,A.style "height" "100vh"
         ,A.style "padding-top" "36px"
         ,A.style "width" "100vw"
         ,A.style "min-width" "720px"
         ,A.style "display" "flex"
        ]
        [ div [ A.class "table-wrap", A.id "test" 
              ,A.style "flex" "0 0 50%"
              ,A.style "overflow" "auto"
              ,A.style "position" "relative"
              ]
           [ viewTable model]

        , div [ A.class "other-wrap" 
               ,A.style "flex" "1 1 auto"
               ,A.style "padding" "10px"
               ,A.style "box-shadow" "0 0 5px rgba(0, 0, 0, 0.3)"
              ] [ viewCsv model.table ]
        ]
        ]

viewCsv : Table -> Html Msg
viewCsv table =
    div [ A.class "csv-editor" ]
        [ h2 [ A.class "title" ] [ text "CSV" ]
        , textarea
            [ A.class "editor"
            , A.cols 80
            , A.rows 20
            , E.onInput EditCsv
            , E.onFocus <| Select Csv
            , E.onBlur <| Select <| Cell <| CellRef 0 0
            ]
            [ text <| toCsv table ]
        ]


viewTable : Model -> Html Msg
viewTable model =
    let
       colstart = model.viewrange.colstart
       colend  = model.viewrange.colend
       rowstart = model.viewrange.rowstart
       rowend  = model.viewrange.rowend
    in
    table [ A.class "table-editor" ] <|
        viewColHeader (.col <| getTableSize model.table) colstart colend model.table.attr_cols model.input
            --:: List.indexedMap (lazy3 viewRow (model.input, model.table.attrs, (model.table.attr_cols, model.table.attr_rows))) (Array.toList model.table.cells)
            --<SCOPE>--
            --:: List.indexedMap (lazy3 viewRow (model.input, model.table.attrs, (model.table.attr_cols, model.table.attr_rows))) (Array.toList (Array.slice 2 6 model.table.cells))
            :: List.indexedMap (lazy3 viewRow (model.input, model.table.attrs, ((model.table.attr_cols, model.table.attr_rows),( (colstart,colend), (rowstart, rowend)))))  (Array.toList (Array.slice ((-) rowstart 1)  rowend model.table.cells))



materialIcon : String -> Html Msg
materialIcon iconName =
    i [ A.class "material-icons" ] [ text iconName ]


viewCtlPanel :  InputState -> Html Msg
viewCtlPanel   input =
   let
     selectedCol =
         case input.selected of
             Cell { col } ->
                 col

             _ ->
                 -1

     selectedRow =
         case input.selected of
             Cell { row } ->
                 row

             _ ->
                 -1

     buttons : Html Msg
     buttons =
        div []
        [
        div []
         [
         span [ A.class "col-operate-buttons" ]
             [ button [ E.onClick <| AddCol selectedCol ] [ materialIcon "col add" ]
             , button [ E.onClick <| DeleteCol selectedCol ] [ materialIcon "col delete" ]
             , button [ E.onClick <| AddCol <| selectedCol + 1 ] [ materialIcon "col add" ]
             ]
          ]
          ,
        div []
         [
         span [ A.class "row-operate-buttons" ]
             [ button [ E.onClick <| AddRow selectedRow ] [ materialIcon "row add" ]
             , button [ E.onClick <| DeleteRow selectedRow ] [ materialIcon "row delete" ]
             , button [ E.onClick <| AddRow <| selectedRow + 1 ] [ materialIcon "row add" ]
             ]
         ]
       ]
   in
   buttons

viewColHeader : Int -> Int -> Int -> Array AttrCol -> InputState -> Html Msg
viewColHeader colSize colstart colend cols input =
    let
        rowIndexToAlphabet : Int -> String
        rowIndexToAlphabet index =
            let
                codeOfA =
                    Char.toCode 'A'

                rightString =
                    String.fromChar <| Char.fromCode <| codeOfA + modBy 26 index
            in
            if index < 26 then
                rightString

            else
                (rowIndexToAlphabet <| index // 26 - 1) ++ rightString

        selectedCol =
            case input.selected of
                Cell { col } ->
                    --col
                    --<SCOPE>--
                    --col + 1
                    col + (-) colstart 1

                _ ->
                    -1

        getCollWidth : Array AttrCol -> Int -> String
        getCollWidth attr_cols_ col_ =
               let
                   attr_col = Array.get col_ attr_cols_
                   width = case attr_col of
                        Just c ->
                            c.width
                        _ ->
                            100.0  -- default call width
               in
                   String.fromFloat width ++ "px"

        headerCell : Int -> Html Msg
        headerCell col =
            td [ A.class "col-header-cell" 
                ,A.style "background-color" "#56cead" --"#ccc"
                ,A.style "font-weight" "normal"
                ,A.style "padding" "5px"
                ,A.style "position" "sticky"
                ,A.style "top" "0px" -- "30px"
                --,A.style "width" "100px" 
                ,A.style "width"  (getCollWidth cols col)
               ]
                [ span [ A.class "inner" 
                        ,A.style "display" "inline-block"
                        ,A.style "text-align" "center"
                        ,A.style "width" "100%"
                        ,A.style "height" "100%"
                        ,A.style "position" "relative"
                       ]
                    [ if col == selectedCol then
                        text "*"
                      else
                        text ""
                    , span [] [ text <| rowIndexToAlphabet col ]
                    ]
                ]
    in
    tr [ A.class "col-header" ] <|
        td [] []
            :: List.map
                headerCell
                 --(List.range 0 (colSize - 1)) 
                 --<SCOPE>--
                 -- (List.filter (\x -> 4 > x && x > 0) (List.range 0 (colSize - 1)) )
                 (List.filter (\x -> colend > x && x > (-) colstart 2) (List.range 0 (colSize - 1)) )


viewRow : (InputState, AttrCells, ((Array AttrCol, Array AttrRow),((Int, Int),( Int, Int)))) -> Int ->  Array Cell -> Html Msg
viewRow (inputState,attrs, ((cols, rows),((colstart, colend), (rowstart, rowend)))) row data =
--viewRow : InputState -> Int -> Array Cell -> Html Msg
--viewRow inputState row data =
    let
        selectedRow =
            case inputState.selected of
                Cell ref ->
                    ref.row

                _ ->
                    -1

        getRowHeight : Array AttrRow -> Int -> String
        getRowHeight rows_ row_ =
               let
                   row_a = Array.get row_ rows_
                   height = case row_a of
                        Just r ->
                            r.height
                        _ ->
                            10.0  -- default row height
               in
                   String.fromFloat height ++ "px"

    in
    tr [ A.class "data-row" ] <|
        th [ A.class "row-header-cell" 
            ,A.style "background-color" "#ccc"
            ,A.style "font-weight" "normal"
            ,A.style "padding" "5px"
            ,A.style "position" "sticky"
            ,A.style "left" "0px" --"30px"
           ]
            [ span [ A.class "inner" 
                    ,A.style "display" "inline-block"
                    ,A.style "text-align" "center"
                    ,A.style "width" "100%"
                    --,A.style "height" "100%"
                    --,A.style "height" "100px"
                    ,A.style "height" (getRowHeight rows row)
                    ,A.style "position" "relative"
                   ]
                [ if row == selectedRow then
                    -- rowOperationButtons selectedRow
                    text "*"

                  else
                    text ""
                --, span [] [ text <| String.fromInt <| row + 1 ]
                                                      --<SCOPE>--
                -- , span [] [ text <| String.fromInt <| row + 1 + 2]
                , span [] [ text <| String.fromInt <| row + rowstart]
                ]
            ]
            --:: List.indexedMap (lazy4 viewCell (inputState, attrs, (cols, rows)) row) (Array.toList data)
            --<SCOPE>--
            :: List.indexedMap (lazy4 viewCell (inputState, attrs, (cols, rows)) row) 
                  -- (Array.toList (Array.slice 1 4 data)) 
                  (Array.toList (Array.slice ((-) colstart 1) colend data)) 


viewCell : (InputState, AttrCells, (Array AttrCol, Array AttrRow)) -> Int -> Int -> Cell -> Html Msg
viewCell (inputState, attrs, (cols, rows)) row col cell =
    let
        cellRef =
            CellRef row col

        isSelected =
            inputState.selected == Cell cellRef

        getBgColor : AttrCells -> Int -> Int -> String
        getBgColor attrs_ row2 col2 =
             let
                tmp = Array.get row2 attrs_
                color = case tmp of
                     Just t ->
                        let
                          tmp2 = Array.get col2 t
                        in
                        case tmp2 of
                           Just a ->
                                a.bg_color
                           _ ->
                                "#FFFFFF"
                     _ ->
                           "#FFFFFF"
              in
                color
             
        getFgColor : AttrCells -> Int -> Int -> String
        getFgColor attrs_ row2 col2 =
             let
                tmp = Array.get row2 attrs_
                color = case tmp of
                     Just t ->
                        let
                          tmp2 = Array.get col2 t
                        in
                        case tmp2 of
                           Just a ->
                                a.fg_color
                           _ ->
                                "#000000"
                     _ ->
                           "#000000"
              in
                color

        getRowHeight : Array AttrRow -> Int -> String
        getRowHeight rows_ row_ =
               let
                   row_a = Array.get row_ rows_
                   height = case row_a of
                        Just r ->
                            r.height
                        _ ->
                            10.0
               in
                   String.fromFloat (height) ++ "px"

        getColWidth : Array AttrCol -> Int -> String
        getColWidth attr_cols_ col_ =
               let
                   attr_col = Array.get col_ attr_cols_
                   width = case attr_col of
                        Just c ->
                            c.width
                        _ ->
                            100.0
               in
                   String.fromFloat (width+3) ++ "px"
                   -- "100px"

    in
    if inputState.isEditing && isSelected then
        -- td [ A.class "cell", A.class "-selected" 
        td [  
             A.style "border" "0.1px solid green"
            ,A.style "padding" "0px"
            ,A.style "margine" "0px"
                  -- ,A.style "valign" "top"
                   ,A.style "position" "relative"
                    , A.style "width"  (getColWidth  cols col)
                    , A.style "height" (getRowHeight rows row)
        
           ]
           {--
            [ form [ E.onSubmit Set
                   ,A.style "valign" "top"
                    --,A.style "border" "1px solid red"
                    --,A.style "border" "0"
                    --, A.style "padding" "0px"
                    --, A.style "margine" "0px"
                    -- , A.style "width"  (getColWidth  cols col)
                    -- , A.style "height" (getRowHeight rows row)
                    --, A.style "width"  "100%"
                    --, A.style "height" "100%"
                   ]
                   --}
                --[ input
                [ textarea
                    [ 
                     E.onSubmit Set
                    , E.onInput Edit
                    , onCtrlKey
                   -- , onEnterKey
                   , A.id "cell-input"
                   --, onKeyPress TextAreaInput
                   --, onKeyPressSkip TextAreaInput  -- ENter code:13 SKIP
                   , A.value inputState.text
                   , A.property "selectionStart" <| JE.int 1
                   , A.property "selectionEnd" <| JE.int 3
                   , A.wrap "soft"
                   ,A.style "position" "relative"
                   ,A.style "resize" "none"
                   ,A.style "outline" "none"  -- Chrome orange line not draw
                    ,A.style ":focus border" "none"           
                    , A.style "padding" "2px"  -- EDIT　TIME
                    , A.style "width"  (getColWidth  cols col)
                    , A.style "height" (getRowHeight rows row)
                    --, A.style "width"  "100%"
                    --, A.style "height" "100%"
                    ]
                    []
                ]

    else
     if isSelected then
        td
            [ A.class "cell"
            , A.class <|
                if isSelected then
                    "-selected"

                else
                    ""
            , A.style "border" "0.01px solid blue"
            , A.style "padding" "0px"
            , A.style "margine" "0px"
            -- , A.style "min-width" "100px"
            , A.style "background-color" (getBgColor attrs row col)
            , A.style "color" (getFgColor attrs row col)
            --, A.style "white-space" "pre-weap"
            , A.style "white-space" "pre"
            , E.onClick (Select <| Cell cellRef)
            , E.onDoubleClick (StartEdit (Just cellRef) <| cellToString cell)
            ]
            --[ text  <| cellToString cell ]
            [span [] [ text  <| cellToString cell ]
            ,span [A.style "color" "green"
                  ,A.style "font-weight" "bold"
                  ] [ text "TAIL"]]

     else
        td
            [ A.class "cell"
            , A.class <|
                if isSelected then
                    "-selected"

                else
                    ""
            , A.style "border" "0.05px solid gray"
            , A.style "padding" "0px"
            , A.style "background-color" (getBgColor attrs row col)
            , A.style "color" (getFgColor attrs row col)
            , A.style "white-space" "pre"
            , E.onClick (Select <| Cell cellRef)
            , E.onDoubleClick (StartEdit (Just cellRef) <| cellToString cell)
            ]
            [ text <| cellToString cell ]

onCtrlKey : Attribute Msg
onCtrlKey =
    let
        func : String -> Msg
        func key =
            --let _ = Debug.log "key:" key in
            if key == "Escape" then
                CancelEdit

            else if key == "Tab" then
                Set

            else
                None
    in
    E.on "keydown" (JD.map func (JD.field "key" JD.string))


{--
onKeyPress : (Int -> Msg) -> Attribute Msg
onKeyPress tagger =
    -- Html.Events.on "keypress" (Json.map tagger Html.Events.keyCode)
    E.on "keypress" (JD.map tagger E.keyCode)

isEnterCode : Int -> Bool
isEnterCode code =
    let
            enterCode =
                        13
    in
            code == enterCode

onKeyPressSkip : (Int -> Msg) -> Attribute Msg
onKeyPressSkip tagger =
    --E.stopPropagationOn  "keydown"
    E.preventDefaultOn  "keypress"
       (JD.map (\x -> ( None, isEnterCode x )) <| E.keyCode)

--}

--onSelection : (Int -> Int) -> Attribute Msg
--onSelection tagger =
 
