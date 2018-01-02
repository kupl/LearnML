let rec fold func l a=
	match l with
	| [] -> a
	| hd::tl -> func hd (fold func tl a);;

let f lst = fold(fun x y -> if(x<=y) then x else y) lst (max lst);;
