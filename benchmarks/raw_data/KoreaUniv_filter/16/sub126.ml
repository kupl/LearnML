let rec filter p l = match l with
	|[] -> []
	|h::t -> if p h then h::filter p t else filter p t;;
