(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
if n<=0 then l else  
match l with
[] -> []
|h::t -> drop t (n-1)