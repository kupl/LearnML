let rec iter(n, f) x = 
	if n > 0 then f( iter( n - 1, f ) x )
	else x
