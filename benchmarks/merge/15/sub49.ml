(*
open Printf
let ex1 : int list = [10; 7; 4; 1]
let ex2 : int list = [9; 8; 3; 2]
let ex3 : int list = [1; 3; 5; 7]

let mirror l =
	let rec aux accu = function
	| [] -> accu
	| h::t -> aux (h::accu) t
in aux [] l

let append addlist addint = mirror (addint::(mirror addlist))

let rec find_min l =
	match l with
	| h::[] -> (h, [])
	| h::t ->
		let (m, l') = find_min t in
		if h < m then (h, t)
		else (m, h::l')

let rec selection_sort testlist =
	match testlist with
	| [] -> []
	| _ ->
		let (m, testlist') = find_min testlist in
		m::(selection_sort testlist')
*)

(* 2013-10894 지구환경과학부 강혁진 1-1번 문제*)
let rec merge (llist, rlist) =
	match llist, rlist with
	| ([], rest) -> rest
	| (rest, []) -> rest
	| hl::tllist, hr::trlist ->
		if hl > hr then hl:: merge (tllist, rlist) else hr:: merge (llist, trlist)


