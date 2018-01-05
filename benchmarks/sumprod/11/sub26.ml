exception InvalidRange

let rec prod(matrix,nn,kk,k) = 
	if(kk==k) then matrix(nn,kk)
	else matrix(nn,kk) *. prod(matrix,nn,kk+1,k)

let rec sum(matrix,nn,n,k) =
	if(nn==n) then prod(matrix,n,1,k)
	else prod(matrix,nn,1,k) +. sum(matrix,nn+1,n,k)


let sumprod(matrix, n, k)
	= sum(matrix,1,n,k)

