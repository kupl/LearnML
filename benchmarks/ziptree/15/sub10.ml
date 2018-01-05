
exception NOMOVE of string

type item = string
type tree = LEAF of item
            | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun(loc) ->
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp: location -> location = fun(loc) ->
    match loc with
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.append left (t::right)), up)

let goDown: location -> location = fun(loc) ->
    match loc with
    | LOC(LEAF(i), z) -> raise (NOMOVE "down of leaf")
    | LOC(NODE(l), z) -> LOC(List.hd(l), HAND([], z, List.tl(l)))

(*
let tree0 = 
    NODE( 
        [NODE ([LEAF ("a"); LEAF ("*"); LEAF ("b")]); LEAF ("+"); 
        NODE([LEAF ("c"); LEAF ("*"); LEAF ("d")])] ) 

let tree1 = 
    NODE 
    ([NODE ( 
        [NODE ([LEAF ("a"); LEAF ("/"); LEAF ("b")]); LEAF ("-"); 
        NODE ([NODE ([LEAF ("c"); LEAF ("*"); LEAF ("d")]); LEAF ("+"); LEAF
        ("e")])]); 
        LEAF ("%"); LEAF ("F")]) 
*)
