exception Invalid;;

let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n1 < 0 || n2 < 0 || n2 > n1 then raise Invalid
	else
		match n2 with
			0 -> 1|
			x when x = n1-> 1|
			_ -> pascal((n1-1),(n2-1)) + pascal((n1-1), (n2));;
