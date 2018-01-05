let sumprod(matrix,n,k)=
	let rec foo(i)=
		let rec bar(j)=
			if(j>k) then 1.
			else matrix(i,j)*.bar(j+1)
		in
		if(i>n) then 0.
		else foo(i+1)+.bar(1)
	in
	foo(1)

