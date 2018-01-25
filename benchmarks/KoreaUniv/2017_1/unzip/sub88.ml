(* problem 7*)
let rec one lst =
match lst with
| [] -> []
| [(x,_)] -> [x]
|(hx,_)::tl -> [hx]@(one tl);;

let rec two lst = 
match lst with
|[] -> []
|[(_,y)] -> [y]
|(_,hy)::tl -> [hy]@(two tl);;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (one lst, two lst);;