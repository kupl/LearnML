exception Error of string

let rec pi(matrix, n, k)=
        if k = 1 then matrix(n,k)
        else matrix(n,k)*.pi(matrix,n,k-1)
;;

let rec sumprod(matrix, n, k)=
        if n <= 0 || k <=0 then raise (Error "input error")
        else(
        if n = 1 then pi(matrix,1,k)
        else pi(matrix,n,k)+.sumprod(matrix,n-1,k)
        )
;;
