type real = float
let rec sumprod : (int * int -> real) * int * int -> real =
	let rec times : (int * int -> real) * int * int * int -> real =
	fun (matrix, n, j, k) ->
		if j = k then
			matrix (n, j)
		else
			(matrix (n, j)) *. (times (matrix, n, j+1,k)) in
	fun (matrix, n, k) -> match n with 
		1 -> times (matrix, n, 1, k)
		|_ -> (times (matrix, n, 1, k)) +. (sumprod (matrix, (n-1), k))

		
		
