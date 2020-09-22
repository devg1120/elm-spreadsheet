module Array2dOp exposing (..)

import Array2D

{--

  defaultCell :  String
  defaultCell = "@"

    array2Dexpand 10 10  defaultCell array2d

    array2Dtrans 0 0  2 2 8 8   defaultCell n12

    array2Dcopy 0 0  2 2 8 8    n12

    array2Dclear 0 0  2 2  defaultCell n12

    array2Dget 0 0  2 2  defaultCell n12

    array2Dset 8 8  n16 n12

    array2Dhjoin   defaultCell n16 n16

    array2Dvjoin   defaultCell n16 n16

--}

---------------------------------------------------

{--
       array2Dexpand
                                    col
       -----                      ----- ------------
      |     |     ==>>           |     |            |
      |     |                    |     |            |
       -----                row   -----             |
                                 |                  |
                                 |                  |
                                 |                  |
                                  ------------------

--}

array2Dexpand : Int -> Int -> a -> Array2D.Array2D a -> Array2D.Array2D a
array2Dexpand rowsnum colsnum fillvalue orgArray2D =
    let
      baseArray2D =
          Array2D.repeat rowsnum colsnum fillvalue

      cellReplace: Int -> Int -> a -> a
      cellReplace row col cell =
          let
            data = Array2D.get row col orgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    in
    Array2D.indexedMap cellReplace baseArray2D


{--
       array2Dtrans
                  col                                   col
         ------------------                    ------------------
        |+----+            |   ==>>           |                  |
        ||    |            |                  |                  |
   row  ||    |            |             row  |                  |
        |+----+            |                  |         +----+   |
        |                  |                  |         |    |   |
        |                  |                  |         |    |   |
         ------------------                    ---------+----+---

--}

array2Dtrans : Int -> Int -> Int -> Int -> Int -> Int -> 
               a -> Array2D.Array2D a -> Array2D.Array2D a
array2Dtrans srcrowindex srccolindex rowsize colsize dstrowindex dstcolindex 
               fillvalue orgArray2D =
    let
      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = Array2D.get (row + srcrowindex)  (col + srccolindex) orgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    
      cellFill: Int -> Int -> a -> a
      cellFill row col cell =
          let
            data = if row >= srcrowindex && row < (srcrowindex + rowsize) then
                     if col >= srccolindex && col < (srccolindex + colsize) then
                         fillvalue
                     else
                         cell
                   else
                      cell

          in
          data
     
      cellSet: Int -> Int -> a -> a
      cellSet row col cell =
          let
            data = Array2D.get (row - dstrowindex) (col - dstcolindex) targetArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell


      -- trans target array2D
      targetArray2D =  Array2D.indexedMap cellGet orgArray2D
      -- base fill
      baseArray2D   = Array2D.indexedMap cellFill orgArray2D

      in
      Array2D.indexedMap cellSet baseArray2D 


{--
       array2Dcopy
                  col                                   col
         ------------------                    ------------------
        |+----+            |   ==>>           |+----+            |
        ||    |            |                  ||    |            |
   row  ||    |            |             row  ||    |            |
        |+----+            |                  |+----+   +----+   |
        |                  |                  |         |    |   |
        |                  |                  |         |    |   |
         ------------------                    ---------+----+---

--}

array2Dcopy : Int -> Int -> Int -> Int -> Int -> Int -> 
               Array2D.Array2D a -> Array2D.Array2D a
array2Dcopy srcrowindex srccolindex rowsize colsize dstrowindex dstcolindex 
               orgArray2D =
    let
      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = Array2D.get (row + srcrowindex)  (col + srccolindex) orgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    
     
      cellSet: Int -> Int -> a -> a
      cellSet row col cell =
          let
            data = Array2D.get (row - dstrowindex) (col - dstcolindex) targetArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell


      -- trans target array2D
      targetArray2D =  Array2D.indexedMap cellGet orgArray2D

      in
      Array2D.indexedMap cellSet orgArray2D 

array2Dclear : Int -> Int -> Int -> Int ->  
               a -> Array2D.Array2D a -> Array2D.Array2D a
array2Dclear srcrowindex srccolindex rowsize colsize  
               fillvalue orgArray2D =
    let
      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = Array2D.get (row + srcrowindex)  (col + srccolindex) orgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    
      cellFill: Int -> Int -> a -> a
      cellFill row col cell =
          let
            data = if row >= srcrowindex && row < (srcrowindex + rowsize) then
                     if col >= srccolindex && col < (srccolindex + colsize) then
                         fillvalue
                     else
                         cell
                   else
                      cell

          in
          data
     
      in
      Array2D.indexedMap cellFill orgArray2D


array2Dget : Int -> Int -> Int -> Int ->  
               a -> Array2D.Array2D a -> Array2D.Array2D a
array2Dget srcrowindex srccolindex rowsnum colsnum
               fillvalue orgArray2D =
    let
      baseArray2D =
          Array2D.repeat rowsnum colsnum fillvalue

      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = Array2D.get (row + srcrowindex)  (col + srccolindex) orgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    

      in
      Array2D.indexedMap cellGet baseArray2D 


array2Dset : Int -> Int ->  
               Array2D.Array2D a -> Array2D.Array2D a
               -> Array2D.Array2D a
array2Dset  dstrowindex dstcolindex 
             targetArray2D  orgArray2D =
    let
    
     
      cellSet: Int -> Int -> a -> a
      cellSet row col cell =
          let
            data = Array2D.get (row - dstrowindex) (col - dstcolindex) targetArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell



      in
      Array2D.indexedMap cellSet orgArray2D 


array2Dhjoin :  a ->
                Array2D.Array2D a -> Array2D.Array2D a
                -> Array2D.Array2D a
array2Dhjoin    fillvalue
                leftorgArray2D rightorgArray2D =
    let
      leftcolms  = Array2D.columns leftorgArray2D
      rightcolms = Array2D.columns rightorgArray2D

      leftrows  = Array2D.rows leftorgArray2D
      rightrows = Array2D.rows rightorgArray2D

      baseArray2D =
          Array2D.repeat leftrows (leftcolms + rightcolms) fillvalue

      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = if col < leftcolms then
                      Array2D.get row  col leftorgArray2D
                  else
                      Array2D.get row  (col - leftcolms ) rightorgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    

      in
      Array2D.indexedMap cellGet baseArray2D 

array2Dvjoin :  a ->
                Array2D.Array2D a -> Array2D.Array2D a
                -> Array2D.Array2D a
array2Dvjoin    fillvalue
                toporgArray2D bottomorgArray2D =
    let
      topcolms  = Array2D.columns toporgArray2D
      bottomcolms = Array2D.columns bottomorgArray2D

      toprows  = Array2D.rows toporgArray2D
      bottomrows = Array2D.rows bottomorgArray2D

      baseArray2D =
          Array2D.repeat (toprows + bottomrows) topcolms  fillvalue

      cellGet: Int -> Int -> a -> a
      cellGet row col cell =
          let
            data = if row < toprows then
                      Array2D.get row  col toporgArray2D
                  else
                      Array2D.get (row - toprows )  col  bottomorgArray2D
          in
            case data of
                Nothing ->
                     cell
                Just newcell ->
                     newcell
    

      in
      Array2D.indexedMap cellGet baseArray2D 
---------------------------------------------------

array2d =
    Array2D.fromList
      [ ["Row 1-Col 1", "Row 1-Col 2"]
      , ["Row 2-Col 1", "Row 2-Col 2"]
      ]

defaultCell :  String
defaultCell = "@"

n12 = 
    array2Dexpand 10 10  defaultCell array2d

n13 = 
    array2Dtrans 0 0  2 2 8 8   defaultCell n12
    --array2Dtrans 0 0  2 2 1 1   defaultCell n12

n14 = 
    array2Dcopy 0 0  2 2 8 8    n12

n15 = 
    array2Dclear 0 0  2 2  defaultCell n12
    --array2Dclear 0 0  2 2  "--" n12

n16 = 
    array2Dget 0 0  2 2  defaultCell n12

n17 = 
    array2Dset 8 8  n16 n12

n18 = 
    array2Dhjoin   defaultCell n16 n16

n19 = 
    array2Dvjoin   defaultCell n16 n16

---------------------------------------------------

---------------------------------------------------
