(*problem 6*)
let drop : 'a list -> int -> 'a list
= fun l n ->
let rec dro l n = 
if n = 0 then l
else
match l with
| [] -> []
| hd :: tl -> dro tl (n-1) in dro l n;;