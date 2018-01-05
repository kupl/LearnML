let rec sum_col(mat, n, k) =
        match k with
        | 1 -> mat(n, 1)
        | _ -> mat(n, k) *. sum_col(mat, n, k-1)


let rec sumprod (mat, n, k) =
	if(n == 0) then 0.0
	else sum_col(mat, n, k) +. sumprod(mat, n-1, k)

