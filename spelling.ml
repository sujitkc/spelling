(*
  Coordinate module for creating set of coordinates
*)
module Coordinate = struct
  type coordinate = {
    row    : int;
    column : int
  }

  type t = coordinate

  let compare c1 c2 =
    let m1 = c1.row * c1.row + c1.column * c1.column
    and  m2 = c2.row * c2.row + c2.column * c2.column
    in
      if m1 > m2 then 1
      else if m1 = m2 then 0
      else -1
end

module CoordinateSet = Set.Make(Coordinate)

let set_of_list lst =
  List.fold_right CoordinateSet.add lst CoordinateSet.empty

type orientation =
    HORIZONTAL
  | VERTICAL

type position = {
  coord  : Coordinate.coordinate;
  orient : orientation
}

type entry = {
  str : string;
  pos : position
}

let setValue m r c v =
  m.(r).(c) <- v

let initMatrix m =
  let randomChar () =
    let alphabets = [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
       'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'
    |]
    in alphabets.(Random.int (Array.length alphabets))
  in
  for r = 0 to (Array.length m - 1) do
    for c = 0 to (Array.length m) - 1 do
      (setValue m r c (randomChar()))
    done
  done

let generateRandomEntry m s =
  let generateRandomPosition () =
    let (r, c, o) = 
      if (Random.int 2) == 0 then
      (
        (Random.int (Array.length m)),
        (Random.int (Array.length m - String.length s)),
        HORIZONTAL
      )
      else
      (
        (Random.int (Array.length m - String.length s)),
        (Random.int (Array.length m)),
        VERTICAL
      )
    in
    {
      coord  = {
        Coordinate.row    = r;
        Coordinate.column = c
      };
      orient = o
    }
  in
  {
    str = s;
    pos = generateRandomPosition ()
  }

(*
  Given an orientation, return a function that will give the appropriate
  next position for a given position. This returned function should return
  the next position to the right of the given position if orient is 
  HORIZONTAL. However, if orient is VERTICAL, this function should return
  the next position below the given position.
*)
let nextPosition orient =
  if orient == HORIZONTAL then
    fun p -> 
      let c = { p.coord with Coordinate.column = p.coord.Coordinate.column + 1}
      in
      { p with coord = c}
  else
    fun p ->
      let c = { p.coord with Coordinate.row = p.coord.Coordinate.row + 1}
      in
      { p with coord = c}

(*
  Given an entry e, return the list of coordinates which e occupies.
*)
let getAllCoordinates e =
  let np = (nextPosition e.pos.orient)
  in
    let rec iter p i =
      if i == (String.length e.str) then []
      else
        let nextpos = (np p)
        in
        p.coord :: (iter nextpos (i + 1))
  in
  iter e.pos 0
  
(*
  Place the entry en in the matrix mat.
*)
let placeEntry mat en =
  let str = en.str and pos = en.pos
  in
    let np = nextPosition pos.orient
    in
      let rec iter p i =
        if i = (String.length str) then
          ()
        else
        begin
          (setValue mat (p.coord.Coordinate.row) (p.coord.Coordinate.column) str.[i]);
          (iter (np p) (i + 1))
        end
      in
      iter pos 0

let getNonConflictingEntry matrix st entries =
  let coordinates = List.map getAllCoordinates entries
  in
  (*
    Return true if c CoordinateSet overlaps with any of the elements of coordinates; otherwise return false.
  *)
  let conflict c =
    let rec iter = function
        [] -> false
      | h :: t -> ((CoordinateSet.inter (set_of_list c) (set_of_list h)) <> CoordinateSet.empty) || (iter t)
    in iter coordinates
  in
    let rec iter s e =
      let e' = (generateRandomEntry matrix s)
      in
        if (not (conflict (getAllCoordinates e'))) then e'
        else iter st e
  in
  iter st entries

(*
  Given a matrix and a string list slist, return a list of entries, one entry corresponding to each of the strings
  in slist. The entries are randomly generated and do not overlap with each other.
*)
let generateEntries matrix slist =
  let rec iter = function
      [] -> []
    | h :: t ->
      let tentries = iter t
      in
        (getNonConflictingEntry matrix h tentries) :: tentries
  in
  iter slist

(*
  Return two matrixes qmatrix (question matrix) and amatrix (answer matrix). Both these matrices contain all the strings
  in the slist list of string. These strings are positioned and oriented randomly in 
  both matrices. However, the position and orientation of each string is identical in
  both matrices. In all other positions, unoccupied by any of the strings of slist,
  qmatrix contains randomly generated letters. In these locations, amatrix contains
  '*'. 
*)
let placeAllStrings slist =
  let qmatrix = [|
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |]
  |]
  and amatrix = [|
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |];
    [| '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*'; '*' |]
  |]
  in
    let rec iter m = function
      [] -> ()
      | h :: t -> (placeEntry m h); (iter m t)
    in
    begin
      initMatrix qmatrix;
      let entries = (generateEntries qmatrix slist)
      in
      begin
        iter qmatrix entries;
        iter amatrix entries
      end
    end;
    (qmatrix, amatrix)

let printMatrix matrix =
  for r = 0 to (Array.length matrix) - 1 do
    for c = 0 to (Array.length matrix) - 1 do
      Printf.printf "%c " matrix.(r).(c)
    done;
    print_string "\n"
  done

let test_placeEntry () =
  let m = [|
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |];
    [| '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@'; '@' |]
  |]
  and e1 = {
    str = "Sujit";
    pos = {
      coord = {
        Coordinate.row    = 3;
        Coordinate.column = 4
      };
      orient = HORIZONTAL
    }
  }
  and e2 = {
    str = "Shilpi";
    pos = {
      coord = {
        Coordinate.row    = 2;
        Coordinate.column = 6
      };
      orient = VERTICAL
    }
  }


  in
  placeEntry m e1;
  placeEntry m e2;
  printMatrix m
 
let t1 () =
  let sl = [ "dog"; "cat"; "tiger"; "man"; "zebra"; "parrot" ]
  in
    let (q, a) = placeAllStrings sl
    in
    begin
      printMatrix q;
      print_string "\n";
      printMatrix a
    end
