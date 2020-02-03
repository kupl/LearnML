(*problem 1*)
let rec fold f l a =
match l with
| [] -> a
| hd :: tl -> f hd (fold f tl a)

let rec max: int list -> int
	= fun l-> match l with
| h :: t -> 
 fold (fun a b -> if a>b then a else b) l h;;
 