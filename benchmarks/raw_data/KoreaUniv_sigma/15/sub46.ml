(* Problem 1 *)
exception Invalid_position;;

let rec sigma f a b =
	if ( a > b ) then
		0
	else if ( a = b ) then
		f a
	else
		f a + sigma f (a+1) b;;
