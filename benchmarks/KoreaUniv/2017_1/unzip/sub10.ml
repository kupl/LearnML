
(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec loop lst = 
match lst with
| [] -> ([],[])
| hd::tl -> (fun (x,y) (a,b) -> (x::a,y::b)) hd (loop tl)
in (loop lst)
