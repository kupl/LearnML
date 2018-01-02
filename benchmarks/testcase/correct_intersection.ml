{
	[1;2;2;3;4;4;3] , [2;3] -> [2;2;3;3];
}
let rec is_exist h lst =
	match lst with
	| hd::tl -> if(h=hd) then true else (is_exist h tl)
	| [] -> false ;;

let rec f l1 l2 =
	match l1 with
	| hd::tl -> if (is_exist hd l2) then hd::(f tl l2) else (f tl l2)
	| [] -> [];;