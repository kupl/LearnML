{
	[1;3;5;4;3] , [3;5;6;6;4] -> [1;4;6;5;3];
}
let rec find elm lst =
	match lst with
	|[] -> false
	|hd::tl -> if(hd=tl) then true else (find elm tl);;

let rec helper lst =
	match lst with
	|[] -> []
	|hd::tl -> if find hd tl then helper tl else hd::(helper tl) ;;

let remove_dup l1 l2 =
	let lst = l1@l2 in
	helper lst;;