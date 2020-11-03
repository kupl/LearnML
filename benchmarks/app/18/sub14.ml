let rec checkIfIn : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
  | [] -> false
  | hd::tl -> if (a = hd) then true else (checkIfIn a tl)
;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> match (checkIfIn hd l2) with
              | true -> app tl l2
              | false -> app tl (l2 @ [hd])
;;


(*
User-defined function [checkIfIn] takes one single value and a list.

It goes through the given list and checks if the given value [a] is in [lst].
*)