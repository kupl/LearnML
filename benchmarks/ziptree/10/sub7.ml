exception NOMOVE of string

type item = string;;
type tree =
  | LEAF of item
  | NODE of tree list;;
type zipper =
  | TOP
  | HAND of tree list * zipper * tree list;;
type location = LOC of tree * zipper;;

let goLeft =
  function
  | LOC (_, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l :: left, up, right)) -> LOC (l, HAND (left, up, t :: right))
  | LOC (_, HAND ([], _, _)) -> raise (NOMOVE "left of first");;

let goRight =
  function
  | LOC (_, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r :: right)) -> LOC (r, HAND (t :: left, up, right))
  | LOC (_, HAND (_, _, [])) -> raise (NOMOVE "right of first");;

let goUp =
  function
  | LOC (_, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND (left, up, right)) ->
    LOC (NODE (List.rev_append left (t :: right)), up);;

let goDown =
  function
  | LOC (LEAF _, _) -> raise (NOMOVE "down of leaf")
  | LOC (NODE (t :: ts), zip) -> LOC (t, HAND ([], zip, ts))
  | LOC (NODE [], _) -> raise (NOMOVE "down of empty child");;
