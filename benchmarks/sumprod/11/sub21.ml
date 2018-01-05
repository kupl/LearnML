let rec sumprod((matrix: (int * int -> float)), (n:int), (k:int)) =
    let rec sum(matrix,n,k,i,j) =
        let rec prod(matrix,n,k,i,j) =
            if j < k then
                matrix(i,j) *. prod(matrix,n,k,i,j+1)
            else
                matrix(i,j)
        in
        if i < n then
            prod(matrix,n,k,i,j) +. sum(matrix,n,k,i+1,j)   
            else 
                prod(matrix,n,k,i,j)

        in
        sum(matrix,n,k,1,1)

