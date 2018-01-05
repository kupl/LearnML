(* PL HW1-3 "합곱"
   2007-11738
   알렉산더 *)


(* sumprod: (int * int -> float) * int * int -> float *)
let rec sumprod(matrix, n, k)=
    let rec prod(mtrx, i, j) =
        if j=1 then mtrx(i, j)
        else mtrx(i, j) *. (prod (mtrx, i, j-1))
    in
    
    if n=1 then prod(matrix, n, k)
    else prod(matrix, n, k) +. sumprod(matrix, n-1, k)

