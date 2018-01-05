let rec sumprod = (fun (f, n, k) ->
				let rec sumprod2 = (fun (i, j) -> if j > k then raise Exit
								else if j = k then (f (i, k))
								else (f (i, j)) *. (sumprod2 (i, j +1))
					) in
				let rec sumprod3 = (fun i -> if i > n then raise Exit
								else if i = n then (sumprod2 (i, 1))
								else (sumprod2 (i, 1)) +. (sumprod3 (i +1))
					) in
				sumprod3 1
	);;