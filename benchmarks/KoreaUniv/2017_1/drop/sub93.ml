(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> match | with
[] -> if n<=0 
then [] else 
| h::t -> 
if n = 0 then l else drop (n-1) t;;