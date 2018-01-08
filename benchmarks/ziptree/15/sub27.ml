type item = string
type tree =
  | LEAF of item
  | NODE of tree list

type zipper =
  | TOP
  | HAND of tree list * zipper * tree list

type location =
  LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of end")

let goUp loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND(left, up, right)) -> LOC(NODE((List.rev left) @ (t::right)), up)

let goDown loc =
  match loc with
  | LOC(LEAF l, _) -> raise(NOMOVE "down of item")
  | LOC(NODE(t::otree), z) -> LOC(t, HAND([], z, otree))
  | _ -> raise(NOMOVE "down of end")


(* TEST CASE *)
(*
 
let _ =
  let z1 = LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) in
  goLeft z1;;

let _ =
  let z1 = LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) in
  goRight z1;;

let _ =
  let z1 = LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) in
  goUp z1;;

let _ =
  let z1 = LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) in
  goDown (goUp z1);;

*)