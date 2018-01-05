let sumprod (matrix, n, k) =
	let rec gop i j=
		if j=k then matrix(i,j) else matrix(i,j)*.gop i (j+1)
	in	
	let rec hap i =
		if i=n then gop i 1 else gop i 1+.hap (i+1)
	in
	(hap 1)
;;
	