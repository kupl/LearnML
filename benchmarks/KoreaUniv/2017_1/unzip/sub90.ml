(* problem 7 완료 *)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with 
 [] -> ([], [])
| hd :: tl -> 
	let fst (x,_) = x 
		in let snd (_,x) = x 
			in ((fst hd)::(fst (unzip tl)), (snd hd)::(snd (unzip tl)))
;;