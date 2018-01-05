(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제1-2 *)
exception Error of string
let rec prod (matrix, n, k) =
    if k < 1 then raise(Error "k is less than 1")
    else if k = 1 then matrix(n, k)
    else matrix(n, k) *. prod(matrix, n, k-1)
let rec sumprod (matrix, n, k) =
    if n < 1 then raise(Error "n is less than 1")
    else if n = 1 then prod(matrix, n, k)
    else prod(matrix, n, k) +. sumprod(matrix, n-1, k)