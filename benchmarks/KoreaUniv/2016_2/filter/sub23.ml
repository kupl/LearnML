let rec filter pred lst =
	begin match lst with
		| [] -> []
		| hd :: rest -> if (pred hd) then hd :: (filter pred rest)
									  else filter pred rest
	end;;
