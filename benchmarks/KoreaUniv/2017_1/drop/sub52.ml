(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
match l with
  [] -> []
  |h::t -> if n = 0 then h::t else (drop t (n-1))