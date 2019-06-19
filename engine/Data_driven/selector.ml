open Lang
open Util

module A = struct
	
	(* summary of function body *)
	type summary = 
		| F of pat list	(* Flat match *)
		| N of (pat * summary) list (* Nested match *)

	(* Function = (function name, type, summary) *)
	type t = (string * typ * summary) list

	(* To string *)
	let rec string_of_summary : summary -> string
	= fun summary ->
		"match {" ^
		match summary with 
		| F ps -> List.fold_left (fun acc p -> acc ^ Print.pat_to_string p ^ " | ") "" ps
		| N summaries -> List.fold_left (fun acc (p, s) -> acc ^ Print.pat_to_string p ^ ":" ^ string_of_summary s ^ " | ") "" summaries
		^ " }"

	let string_of_t : t -> string
	= fun t ->
		List.fold_left (fun acc (name, typ, summary) -> 
			acc ^ "(" ^
			"Name : " ^ name ^ "\n" ^
			"Type : " ^ Print.type_to_string typ ^ "\n" ^
			"Structure : \n" ^ string_of_summary summary ^ "\n)"
		) "" t

	(* Summarize a given program *)
	let run : prog -> t
	= fun pgm -> []
end

let get_summary : prog -> A.t
= fun pgm -> 
	let summary = A.run pgm in
	summary

(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> prog list -> prog
= fun pgm cpgms -> 
	let cpgm = List.hd cpgms in
	cpgm
