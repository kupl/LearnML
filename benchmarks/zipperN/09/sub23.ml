let rec zipperN list:int list = 
	let rec split list = 
  		match list with
			sublist::lista -> (match sublist with x::sublista -> sublista::split lista | _ -> split lista)
  			| _ -> []
	in
	let rec first list = 
  		match list with
			sublist::lista -> (match sublist with x::sublista -> x::first lista | _ -> first lista)
			| _ -> []
	in	
	if list = [] then []
	else (first list)@(zipperN (split list));;