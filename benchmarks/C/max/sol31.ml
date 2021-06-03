exception Empty_list;;

let rec max l = 
	match l with
	|[] -> raise Empty_list
	|h::[] -> (0+h) (* to make type int *)
	|h::t -> if (h > max t) then h else max t;;
 