type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft: location -> location = fun (loc) ->
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
  | LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun (loc) ->
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
  | LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")

let goUp: location -> location = fun (loc) ->
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND (left, up, right)) -> LOC (NODE (List.rev_append left (t :: right)), up)

let goDown: location -> location = fun (loc) ->
  match loc with
  | LOC (t, z) ->
    (match t with
    | LEAF i -> raise (NOMOVE "down of leaf")
    | NODE [] -> raise (NOMOVE "irregular expression")
    | NODE (hd :: tl) -> LOC (hd, HAND ([], z, tl))
    )