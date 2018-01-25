(* Problem 1 *)
exception Invalid_position;;

let rec pascal (x, y)  =
	if (y = 0 || x = y) then
		1
	else if (y < 0 || x < y || x < 0) then
		raise Invalid_position
	else
		(pascal(x-1, y) + pascal(x-1, y-1));;
