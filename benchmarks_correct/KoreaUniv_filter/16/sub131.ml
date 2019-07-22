let rec filter pred lst = (* TODO *)
	match lst with
	|[] -> []
	|head::tail -> if pred head then head::(filter pred tail)
				   else filter pred tail
