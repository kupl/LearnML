let rec sigma (s : int) (e : int) (fn : int -> int) : int =
	if ( s > e )
		then 0
		else (fn s) + (sigma (s+1) e fn)
	
