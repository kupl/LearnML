let rec zipperN lst:int list = 
	let rec split lst = 
  		match lst with
			sublist::lista -> (match sublist with x::sublista -> sublista::split lista | _ -> split lista)
  			| _ -> []
	in
	let rec first lst = 
  		match lst with
			sublist::lista -> (match sublist with x::sublista -> x::first lista | _ -> first lista)
			| _ -> []
	in	
	if lst = [] then []
	else (first lst)@(zipperN (split lst));;
