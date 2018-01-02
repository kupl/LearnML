let who_is_S x y = if x>y then y else x;;

let rec fold func l a =
	match l with
	| []->1
	| hd::tl -> func hd (fold func tl a);;

let f lst = fold(who_is_S) lst 1;;