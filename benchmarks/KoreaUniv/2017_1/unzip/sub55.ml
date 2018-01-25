let rec unzip : ('a * 'b) list -> 'a list * ' b list
= fun lst -> let fst p = match p with (x,_) -> x in
			let sec p = match p with (_,x) -> x in
			match lst with
			|[]->([],[])
			|[(x,y)] -> ([x],[y])
			|hd::tl -> (fst hd::fst (unzip tl),sec hd::sec (unzip tl))