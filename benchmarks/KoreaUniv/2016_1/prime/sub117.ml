let rec prime  = 
	let rec prime_iter x y =

        if y =1 then true
        else if y = 0 then false 
        else if x mod y = 0 then false
        else prime_iter x (y-1) in
    fun x -> prime_iter x (x-1)
