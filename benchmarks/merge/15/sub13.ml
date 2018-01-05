exception TODO
open Printf

let merge (a, b) =
	let rec insert (a, l) =
		match l with
		| [] -> [a]
		| hd :: tl -> if a > hd then a :: hd :: tl
			      else hd :: insert (a, tl) in
	let rec sort l = 
		match l with
		| [] -> []
		| hd :: tl -> insert (hd, (sort tl)) in
	sort (a @ b)
