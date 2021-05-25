(*
 * Programming Languages, 2013 Fall.
 * HW Code for Exercise 2-2
 * Department of Computer Science and Engineering
 * 2006-11855, Jung Yonghyuk (ever103@snu.ac.kr)
 *)

type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

(* diff: aexp * string -> aexp *)
let rec diff (aexp, s) =
	match aexp with
	| Const i -> Const 0
	| Var s' ->
		if s = s' then Const 1
		else Const 0
	| Power (s', i) ->
		if s = s' then Times [(Const i) ; (Power (s, (i-1)))]
		else Const 0
	| Sum al ->
		(match al with
		| [] -> raise InvalidArgument
		| _ -> Sum (List.map (fun x -> diff (x, s)) al)
		)
	| Times al ->
		(match al with
		| [] -> raise InvalidArgument
		| h::[] -> diff (h, s)
		| h::t ->
			let f = h in
			let f' = diff (f, s) in
			let g = Times t in
			let g' = diff (g, s) in
			Sum [Times [f'; g]; Times [f; g']])
