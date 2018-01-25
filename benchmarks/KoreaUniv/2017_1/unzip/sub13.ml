
(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with 
| [] -> ([], [])
| (x,y) :: tp ->
	let tpl = unzip tp in
	((x::(fst tpl)), (y::(snd tpl)))
