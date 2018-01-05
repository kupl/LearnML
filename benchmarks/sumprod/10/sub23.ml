let e(i,j) = 
	match (i,j) with
	(1,1) -> 1.0
	|(1,2) -> 2.0
	|(1,3) -> 3.0
	|(2,1) -> 4.0
	|(2,2) -> 5.0
	|(2,3) -> 6.0
	|(3,1) -> 7.0
	|(3,2) -> 8.0
	|(3,3) -> 9.0
	|(_,_) -> 0.0

let rec sumprod(a, n, k) =
	let rec pi a(n,k) = 
		if k = 1 then a(n,k)
		else a(n,k) *. (pi a(n,(k-1))) in
	if n = 1 then pi a(n,k)
	else pi a(n,k) +. sumprod(a, (n-1), k)
