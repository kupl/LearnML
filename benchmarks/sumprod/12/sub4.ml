let sumprod(mat, n, k)=
  let rec prod : int*int*int*int*(int*int->float)->float = fun( i, j, n, k, mat ) -> 
    if j=k||k=1 then mat( i, j )
	 else mat(i, j)*.prod(i, j+1,n, k, mat) in
	let rec sum : int*int*int*int*(int*int->float)->float = fun( i, j, n, k, mat ) ->
	   if i=n||n=1 then prod( i, j, n, k, mat )
	   else prod( i, j, n, k, mat )+. sum(i+1, j, n, k, mat ) in
	if n>=1&&k>=1 then sum(1,1,n,k, mat)
  else 0.0