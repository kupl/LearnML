let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
  [] -> [] @ b
| ah::at ->	match b with
 			  [] -> [] @ a
			| bh::bt -> [] @ [ah;bh] @ zipper (at, bt);;

