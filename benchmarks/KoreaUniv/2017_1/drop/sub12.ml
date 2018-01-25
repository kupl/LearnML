(* problem 6 *)
let rec length l =
match l with
|[] -> 0
|hd::tl -> 1 + (length tl)

let rec drop : 'a list -> int -> 'a list = fun l n -> 
match l with
|[] -> []
|hd::tl -> if n=0 then l
else if n>=length l then []
else drop tl (n-1)
