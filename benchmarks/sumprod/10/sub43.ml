exception Error of string
let sumprod (matrix, n, k) =
	let rec sum (matrix, n, k, index2) =
		let rec prod (matrix, i, k, index1) = 
			if index1 = k then matrix (i, k)
			else
				if index1 < k then (matrix (i, index1)) *. prod (matrix, i, k, index1+1)
				else raise (Error "Boundary Error")
		in
		if index2 = n then prod (matrix, n, k, 1)
		else
			if index2 < n then prod (matrix, index2, k, 1) +. sum(matrix, n, k, index2+1)
			else raise (Error "Boundary Error")
	in
	sum (matrix, n, k, 1)

;;

		
				
	
