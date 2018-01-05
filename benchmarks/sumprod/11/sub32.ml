let rec prod(matrix,n,k)=
        if k>0 then matrix(n,k)*.prod(matrix,n,k-1)
        else if k=0 then 1.0
        else raise(Invalid_argument "2nd and 3rd argu must be bigger
        than 0!")
let rec sumprod(matrix,n,k)=
        if k>0&n>0 then prod(matrix,n,k)+.sumprod(matrix,n-1,k)
        else if n=0 then 0.0
        else raise(Invalid_argument "2nd and 3rd argu must be bigger
        than 0!")        
