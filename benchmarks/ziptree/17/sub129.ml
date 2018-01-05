exception NOMOVE of string
    
type item = string
type tree = LEAF of item
          | NODE of tree list
                

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper


let goLeft loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goUp : location -> location  = function loc ->
match loc with
| LOC(t, TOP) -> raise (NOMOVE "top of top")
| LOC(t, HAND (left, up, right)) ->
  LOC (NODE (left @ [t] @ right), up)

let goRight : location -> location = function loc ->
match loc with
| LOC (t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of the first")

let goDown : location -> location  = function loc ->
match loc with
| LOC (LEAF t, TOP) -> raise (NOMOVE "single node tree")
| LOC (NODE t, TOP) ->
  (match t with
  | [] -> raise (NOMOVE "empty Tree")
  | h::t -> LOC (h, HAND([], TOP, t)))
| LOC (LEAF t , HAND (x, y, z)) -> raise (NOMOVE "bottom of leaf")
| LOC (NODE (h::t), k)-> LOC (h, HAND ([], k, t ))
| LOC (NODE [], _ ) -> raise (NOMOVE "NOT Valid Tree")
