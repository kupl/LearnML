open Lang
open Util

(*
	Input : An incorrect program pgm, a correct program cpgm, and a set of testcases testcases
	Output : A repaired program pgm' satisfying all testcases
*)
let run : prog -> prog -> examples -> prog
= fun pgm cpgm testcases -> 
	let pgm' = cpgm in
	pgm'