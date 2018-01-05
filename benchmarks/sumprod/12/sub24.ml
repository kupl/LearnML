let prod (mat, i, k) =
  let rec prod' (mat, i, j, k) =
    if j = k then mat(i/1,j/1)
    else mat(i,j) *. (prod' (mat, i, j+1, k))
  in
  prod' (mat, i, 1, k)


let sumprod (mat, n, k) =
  let rec sumprod' (mat, n, i) =
    if i = n then prod(mat, i, k)
    else prod(mat, i, k) +. sumprod'(mat, n, i+1)
  in
  sumprod' (mat, n, 1)

  (*
let _ = print_float (sumprod ((fun (x,y) -> float_of_int x +. float_of_int y ),
2, 5))
*)
