(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (
  match lst with
  |h::[] -> h
  |h::t -> find h t)
  and find x y =
  match y with
  |[] -> x
  |h::t -> if x <= h then find h t else find x t
;;
 