let under_Two n = if n<2 then true else false;;
let is_Two n = if n=2 then true else false;;

let rec prime n = 
	if under_Two n then false
	else if is_Two n then true
	else	
		let rec innerFun k =
				(n mod k <> 0 && innerFun(k+1)) || k*k>n
				in innerFun 2 && n<>1;;
