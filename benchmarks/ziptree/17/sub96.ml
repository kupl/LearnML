(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-5 *)
type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc = match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight: location -> location = fun loc -> match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, r::right)) ->
        LOC(r, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp: location -> location = fun loc -> match loc with
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
    | LOC(t, HAND(left, up, right)) -> LOC(NODE (List.append (List.rev (t::left)) right), up)

let goDown: location -> location = fun loc -> match loc with
    | LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
    | LOC(NODE (d::down), up) -> LOC(d, HAND([], up, down))
    | LOC(NODE [], _) -> raise (NOMOVE  "Not a valid tree")

(* Test Code
let t = NODE [ NODE [LEAF "a"; LEAF "*"; LEAF "b"];
               LEAF "+";
               NODE [LEAF "c"; LEAF "*"; LEAF "d"]
             ]
let top = LOC (t, TOP)
let z1 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
              HAND([],
                   TOP,
                   [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))
let b = LOC (LEAF "b",
             HAND([LEAF "*"; LEAF "a"],
                  HAND([],
                       TOP,
                       [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
                  []))
let m2 = LOC (LEAF "*",
              HAND([LEAF "c"],
                   HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
                        TOP,
                        []),
                   [LEAF "d"]))

let test (l1, l2) = if (l1 = l2) then print_endline("S")
                    else print_endline("U")

let _ = test(goDown(top), z1)
let _ = test(goUp(b), z1)
let _ = test(goRight(goRight(goDown(z1))), b)
let _ = test(goLeft(goLeft(goUp(m2))), z1)
let _ = test(goRight(goRight(z1)), goUp(m2))
let _ = test(goDown(goRight(goRight(z1))), goLeft(m2))
let _ = test(goUp(z1), top)
let _ = print_newline()
let _ = test(goLeft(z1), top)
let _ = test(goUp(top), top)
let _ = test(goLeft(top), top)
let _ = test(goRight(b), m2)
let _ = test(goDown(m2), b)
*)
