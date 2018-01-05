(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-2 *)
let rec prod (matrix,n,k) =
    if k<1 then 1.0
    else matrix(n,k) *. prod(matrix,n,k-1)
let rec sumprod (matrix,n,k) =
    if n<1 then 0.0
    else prod(matrix,n,k) +. sumprod(matrix,n-1,k)