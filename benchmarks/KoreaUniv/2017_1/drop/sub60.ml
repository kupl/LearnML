(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)
match l with 
| [] -> []
| hd::tl -> if n>0 then hd::(drop tl (n-1))
else []
