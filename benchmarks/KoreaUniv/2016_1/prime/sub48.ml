let rec prime : int -> bool
= fun n->
let rec is_not_divisor d =
	d*d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
 n<>1 && is_not_divisor 2;;
