let rec filter pred lst = 
	let rec aux i = function
      | [] -> []
      | h :: t -> if pred h then h :: aux (i+1) t else aux (i+1) t in
    aux 1 lst
