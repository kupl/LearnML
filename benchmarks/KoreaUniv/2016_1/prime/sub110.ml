let prime : int -> bool
= fun n ->
	let n = abs n in
    let rec div d =
    	d * d > n || ( div (d+1) && n mod d <> 0 ) 
  		in n <> 1 && div 2;;
