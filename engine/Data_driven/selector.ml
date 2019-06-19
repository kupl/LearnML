open Lang
open Util

(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> prog list -> prog
= fun pgm cpgms -> 
	let cpgm = List.hd cpgms in
	cpgm
