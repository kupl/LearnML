(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
match l with
| [] -> []
| hd :: tl ->
if n = 0 then l
else if n = 1 then tl
else drop tl (n-1);;