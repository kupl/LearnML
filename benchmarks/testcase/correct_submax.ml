{
	[33;36;-73;15;43;-17;36;-28;-1;21] -> 77;
  [5;-7;2;3;-4;5;2;-7;8;-7] -> 9;
  [-7;4;-3;6;3;-8;3;4] -> 10;
  [2;-6;4;5;-2;6;2;-1] -> 15;
  [1;2;3;4;5] -> 15;
}
let compare a b = if(a>b) then a else b ;;

let rec max_elm : int list -> int
= fun lst ->
	match lst with
	| [] -> 0
	| hd::tl -> compare hd (max_elm tl) ;;

let rec helper : int list -> int list
= fun lst ->
	match lst with
	|[] -> []
	|hd::tl ->
		let max_lst = helper tl in
		let tl_max = begin match max_lst with |[] -> 0 |hd::tl -> hd end in 
		(hd+(compare 0 tl_max))::max_lst ;;

let sub_max : int list -> int
= fun lst ->
	let max_list = helper lst in
	max_elm max_list ;;
