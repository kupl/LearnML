 
let rec max : int list -> int
= fun l -> match l with
|[] -> raise (Failure "error")
| [a] -> a
| a::b::tl -> if a>b then max (a::tl) else max (b::tl)
