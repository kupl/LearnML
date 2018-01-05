let rec sumprod (matrix, n, k) = 
    let rec sum_of_kth_col (matrix, n, k) =
        if n==0 then 0
        else (matrix n k) + (sum_of_kth_col (matrix, n-1, k)) in
    if k==0 then 0
    else sum_of_kth_col (matrix, n, k)
                         + sumprod (matrix, n, k-1)
    
    
