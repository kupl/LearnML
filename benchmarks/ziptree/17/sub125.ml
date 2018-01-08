(*
 * 2017 - 09 - 22
 * PL Homework 2-5
 * Joonmo Yang
*)

exception NOMOVE of string

type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND(l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
  | LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND(left, up, r::right)) -> LOC (r, HAND(t::left, up, right))
  | LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "already on top")
  | LOC (t, HAND(left, up, right)) -> LOC (NODE (List.rev_append left (t::right)), up)

let goDown loc =
  match loc with
  | LOC (LEAF _, _) -> raise (NOMOVE "already on bottom") 
  | LOC (NODE tlist, zip) ->
    ( match tlist with
      | [] -> raise (NOMOVE "empty tree list")
      | hd::tl -> LOC (hd, HAND([], zip, tl))
    )

(* test cases
let x = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+" ; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])) 
let y = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+" ; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP) 
let z = LOC (LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"])) 
let empty = LOC(NODE[], TOP) 

let (|>) f g = g f 

let _ = 
  let test_case : int * bool -> unit = fun (n, x) -> 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  let test_errorcase : int * bool -> unit = fun (n, x) -> 
    let error_check = fun e -> if(e = true) then "OK" else "Failure" in 
    print_endline ("Error Case " ^ string_of_int(n) ^ " : " ^ error_check(x)) in 
  test_case(1, y |> goDown = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))); 
  test_case(2, y |> goDown |> goDown = LOC (LEAF "a", HAND ([], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "*"; LEAF "b"]))); 
  test_case(3, y |> goDown |> goUp |> goDown = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))); 
  test_case(4, y |> goDown |> goDown |> goRight = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"]))); 
  test_case(5, y |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight = LOC (LEAF "b", HAND ([LEAF "*"; LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), []))); 
  test_case(6, y |> goDown |> goRight |> goRight |> goDown |> goRight = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"]))); 
  test_case(7, x |> goDown |> goRight |> goRight |> goUp |> goUp = y); 
  test_case(8, y |> goDown |> goRight |> goRight |> goDown |> goRight = z); 
  test_errorcase(1, try (goUp(y) = z) with NOMOVE _ -> true); 
  test_errorcase(2, try (goDown(empty) = z) with NOMOVE _ -> true)
*)