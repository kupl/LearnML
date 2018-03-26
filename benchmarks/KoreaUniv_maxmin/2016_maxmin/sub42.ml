(* Name : Jungwon Seo / Student ID : 2012210051 *) 

exception Problem

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
 match lst with
 | [] -> raise Problem
 | [hd] -> hd
 | hd::tl ->
 if hd > max tl then hd
 else max tl

let rec min : int list -> int
= fun lst ->
 match lst with
 | [] -> raise Problem
 | [hd] -> hd
 | hd::tl ->
 if hd < min tl then hd
 else min tl
