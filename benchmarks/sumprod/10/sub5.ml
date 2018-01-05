let result = 0;;

let rec sumprod (matrix, n, k) =
  if n = 1 then 0
           else if k = 1 then matrix(n, k) + sumprod(matrix, n-1, k)
                         else matrix(n, k) * sumprod(matrix, n, k-1);;

(*204*)
sumprod((fun (a, b) -> a + b), 3, 3)  