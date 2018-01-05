
(* EXERCISE 5 *)

type item = string

type tree = LEAF of item
	|NODE of tree list

type zipper = TOP
	|HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

(*
let temp = LOC(LEAF "*" ,
		HAND([LEAF "c"],
			HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], 
				TOP, 
				[]),
			[LEAF "d"]))
*)
exception NOMOVE of string

let goLeft : location -> location = fun loc ->
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t :: right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight : location -> location = fun loc ->
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up,  right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
(* goup, goDown not done*)

let goDown : location -> location = fun loc ->
	match loc with
	| LOC(LEAF(item), zipper) -> raise (NOMOVE "down of bottom")
	| LOC(NODE(tree_list), zipper) -> ( match tree_list with
					|h::t -> LOC(h, HAND([],zipper,t))
					| [] -> raise (NOMOVE "down of nothing")					
					)
let goUp : location -> location = fun loc ->
	let rec snoc : tree * tree list -> tree list = fun (tin, tl) ->
		match tl with
		| [] -> tin::[]
		| h :: tail -> h :: snoc (tin, tail)
	in
	let rec rev_list : tree list -> tree list = fun tl_in ->
		match tl_in with
		| [] -> []
		| h::tail -> snoc(h , rev_list (tail))
	in  
	let rec list_concatenate : tree list * tree list -> tree list = fun (l1, l2) ->
		match l1 with
		| [] -> l2
		| h1 :: t1 -> h1 :: list_concatenate (t1, l2)
	in
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "Up of Top")
	(*| LOC (t, HAND(left, up, right)) -> LOC(NODE (NODE(left) :: t :: NODE(right) :: []), up)*)
	| LOC (t, HAND(left, up, right)) -> LOC(NODE (list_concatenate(rev_list(left), list_concatenate(t::[], right))), up)
	(*| LOC (LEAF(item) , HAND(left, up, right)) -> LOC(NODE (list_concatenate(left, list_concatenate(LEAF(item)::[], right))), up)*)

(*
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

(*
let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+";NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP) 

let (|>) g f = f g 

let a91 = loc1 |> goDown 
let a92 = loc1 |> goDown |> goDown 
let a93 = loc1 |> goDown |> goUp |> goDown 
let a94 = loc1 |> goDown |> goDown |> goRight 
let a95 = loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight 
let a96 = loc1 |> goDown |> goRight |> goRight |> goDown |> goRight 
let a97 = 
try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true 
;;*)
