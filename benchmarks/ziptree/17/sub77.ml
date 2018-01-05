type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc = match loc with
  LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
  LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
  LOC(t, TOP) -> raise(NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev_append (t :: left) (right)), up)

let goDown loc = match loc with
  LOC(LEAF(_), _) -> raise(NOMOVE "down of leaf")
  | LOC(NODE(t :: right), zip) -> LOC(t, HAND([], zip, right))
  | LOC(NODE([]), zip) -> raise(NOMOVE "node with no children")

(* TEST *)
(*
let x = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+" ; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])) 
let y = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+" ; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP) 
let empty = LOC(NODE[], TOP) 

let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  test_case(1, goDown(y) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))); 
  test_case(2, goDown(goDown(y)) = LOC (LEAF "a", HAND ([], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "*"; LEAF "b"]))); 
  test_case(3, goDown(goUp(goDown(y))) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))); 
  test_case(4, goRight(goDown(goDown(y))) = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"]))); 
  test_case(5, goRight(goRight(goLeft(goRight(goDown(goDown(y)))))) = LOC (LEAF "b", HAND ([LEAF "*"; LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), []))); 
  test_case(6, goRight(goDown(goRight(goRight(goDown(y))))) = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"]))); 
  test_case(7, goUp(x) = y); 
  test_case(8, goUp(goUp(goRight(goRight(goDown(x))))) = y); 
  test_case(9, try (goUp(y) = empty) with NOMOVE _ -> true); 
  test_case(10, try (goDown(empty) = empty) with NOMOVE _ -> true)
*)
