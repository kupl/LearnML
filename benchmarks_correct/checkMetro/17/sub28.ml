(*
 CSE 2012-11226 Kwak Jin Han
 exercise 4
 *)

type metro = STATION of name
					 | AREA of name * metro
					 | CONNECT of metro * metro
	and name = string

(*
(* true *)
let a = AREA ("a", STATION "a")
let b = AREA ("a", AREA ("a", STATION "a"))
let c = AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "b")))
let d = AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "a")))
(* false *)
let e = AREA ("a", STATION "b")
let f = AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "c")))
let g = AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "c")))
*)

(* checkMetro : metro -> bool *)
let (* rec *) checkMetro m =
	let rec main (m, saveStation) = 
		match m with
		| STATION pname -> 
				if List.mem pname saveStation = true then true
				else false

		| AREA (pname, pmetro) -> 
				main (pmetro, pname::saveStation)

		| CONNECT (leftmetro, rightmetro) ->
				main (leftmetro, saveStation) && main (rightmetro, saveStation) in
	main(m, [])
