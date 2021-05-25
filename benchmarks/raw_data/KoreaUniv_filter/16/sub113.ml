let rec fold2 f l =
	match l with
	| [] -> []
	| hd::tl -> f hd (fold2 f tl)

let rec filter pred lst = fold2 (fun x y -> (if pred x then [x] else []) @ y) lst

