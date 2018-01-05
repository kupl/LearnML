
type item = string
type tree =
  | LEAF of item
  | NODE of tree list
type zipper =
  | TOP
  | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft : location -> location =
  fun loc ->
    match loc with
    | LOC (_, TOP) -> raise (NOMOVE "left of top")
    | LOC (t, HAND (h::l, u, r)) -> LOC (h, HAND (l, u, t::r))
    | LOC (_, HAND ([], _, _)) -> raise (NOMOVE "left of leftmost")

let goRight : location -> location =
  fun loc ->
    match loc with
    | LOC (_, TOP) -> raise (NOMOVE "right of top")
    | LOC (t, HAND (l, u, h::r)) -> LOC (h, HAND (t::l, u, r))
    | LOC (_, HAND (_, _, [])) -> raise (NOMOVE "right of rightmost")

let goUp : location -> location =
  fun loc ->
    match loc with
    | LOC (t, TOP) -> raise (NOMOVE "up of top")
    | LOC (t, HAND (l, u, r)) ->
        LOC (NODE ((List.rev l) @ (t::r)), u)

let goDown : location -> location =
  fun loc ->
    match loc with
    | LOC (LEAF _, _) -> raise (NOMOVE "down of leaf")
    | LOC (NODE (h::t), u) -> LOC (h, HAND ([], u, t))
    | LOC (NODE [], _) -> raise (NOMOVE "down of empty")

