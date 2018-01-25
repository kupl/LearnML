let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
							| [] -> b
							| x::tail -> x::(zipper (b, tail))
