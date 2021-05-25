let rec iter (n, f)=
	let compose test1 test2 = 
		fun x -> test1 (test2 x) in
	if n <= 0 then (fun x -> x)
	else compose (iter ((n-1), f)) f