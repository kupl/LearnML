(*********************)
(* Problem 1: filter *)
let rec filter pred lst =
match lst with
|[]->[]
|hd::tl->if pred hd=true then hd::filter pred tl else filter pred tl;; 

(*(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
 match (a,b) with
 |([],b)->b
 |(a,[])->a
 |(a,b) ->a::b;;*)

