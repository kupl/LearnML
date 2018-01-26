 let rec prime : int -> bool
  = fun n ->
  	let rec div d = 
  		if d*d > n then true
  		else if n mod d <> 0 && div(d+1) then true 
  		else false in 
  	match n with 
  		1 -> false 
  		|_-> div 2
  	;;
