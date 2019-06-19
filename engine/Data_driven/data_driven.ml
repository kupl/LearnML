open Lang
open Util

(*
	Input : An incorrect program pgm, a set of correct programs cpgms, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let run : prog -> prog list -> examples -> prog
= fun pgm cpgms testcases -> 
	(* Find a most similar correct program *)
	let cpgm = Selector.run pgm cpgms in
	let pgm' = Repairer.run pgm cpgm testcases in
	pgm'