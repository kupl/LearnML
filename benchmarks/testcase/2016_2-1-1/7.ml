let who_is_G x y = if x>y then x else y;;

let rec fold func l a =
	match l with
	| []->1
	| hd::tl -> func hd (fold func tl a);;

let f lst = fold(who_is_G) lst 1;;

