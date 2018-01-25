(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->let rec recur first second lst = match lst with
				| [] -> (first, second)
				| hd::tl -> let (x,y)=hd in
							let first = first@[x] in
							let second = second@[y] in
								recur first second tl in
			recur [] [] lst;;