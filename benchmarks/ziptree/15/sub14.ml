(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-4 *)

exception NOMOVE of string

type item = string

type tree = LEAF of item
| NODE of tree list

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper


let goLeft loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "cannot move to left")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([], up, right)) -> raise (NOMOVE "cannot move to left")

let rec goRight: location -> location = fun(loc) ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "cannot move to right")
  | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "cannot move to right")

let rec goUp: location -> location = fun(loc) ->
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "cannot move to up")
  | LOC(t, HAND([], parent, right)) -> LOC(NODE(t::right), parent)
  | LOC(t, HAND(hd::tl, parent, right)) -> LOC(NODE(tl@hd::t::right), parent)

let rec goDown: location -> location = fun(loc) ->
  match loc with
  | LOC(t, z) ->  (
    match t with
    | LEAF l -> raise (NOMOVE "cannot move to down")
    | NODE (hd::tl) ->  LOC(hd, HAND([], z, tl))
    | NODE ([]) ->  raise (NOMOVE ("cannot move to down"))
  )

  (*
let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; 
                      LEAF "+"; 
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
                  TOP) 

let (|>) g f = f g 

let a91 = loc1 |> goDown 
let a92 = loc1 |> goDown |> goDown 
let a93 = loc1 |> goDown |> goUp |> goDown 
let a93 = loc1 |> goDown |> goDown |> goRight 
let a95 = loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight 
let a96 = loc1 |> goDown |> goRight |> goRight |> goDown |> goRight 
let a97 = 
      try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true 
;;

let loc2 = LOC (LEAF "*", 
HAND([LEAF "c"], 
HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
TOP, 
[]), 
[LEAF "d"])) 

let b91 = loc2 |> goLeft
let b92 = loc2 |> goUp
let b93 = loc2 |> goUp |> goUp
*)
