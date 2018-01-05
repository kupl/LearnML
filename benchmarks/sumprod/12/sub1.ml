let rec sumprod : ( ( ( int * int -> float ) * int * int ) -> float ) = fun ( m, n, k ) ->
	let rec product( n, k) = 
			if k>0 then m( n, k ) *. product( n, k-1 )
			else 1.0
	in	
		if n> 0 then product( n, k) +. sumprod( m, n-1, k )
		else 0.0



