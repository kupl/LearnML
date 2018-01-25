
(* problem 7*)
let get_first
= fun l -> match l with
		| (la, _) -> la;;

let get_second
= fun l -> match l with
		| (_, lb) -> lb;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
		| [] -> ([],[])
		| (x,y)::tl -> let res = (unzip tl) in (x::(get_first res), y::(get_second res));;