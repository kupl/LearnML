(* Problem 1 *)

let rec max : int list -> int
=fun l -> match l with
|hd::tl->if hd > max tl then hd else max tl
|[] -> -999
 