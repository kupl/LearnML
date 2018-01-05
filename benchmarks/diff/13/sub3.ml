(*
 * Programming Languages, 2013 Fall.
 * HW Code for Exercise 2-2
 * Department of Computer Science and Engineering
 * 2006-11855, Jung Yonghyuk (ever103@snu.ac.kr)
 *)

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

(* diff: ae * string -> ae *)
let rec diff (ae, s) =
	match ae with
	| CONST i -> CONST 0
	| VAR s' ->
		if s = s' then CONST 1
		else CONST 0
	| POWER (s', i) ->
		if s = s' then TIMES [(CONST i) ; (POWER (s, (i-1)))]
		else CONST 0
	| SUM al ->
		(match al with
		| [] -> raise InvalidArgument
		| _ -> SUM (List.map (fun x -> diff (x, s)) al)
		)
	| TIMES al ->
		(match al with
		| [] -> raise InvalidArgument
		| h::[] -> diff (h, s)
		| h::t ->
			let f = h in
			let f' = diff (f, s) in
			let g = TIMES t in
			let g' = diff (g, s) in
			SUM [TIMES [f'; g]; TIMES [f; g']])
