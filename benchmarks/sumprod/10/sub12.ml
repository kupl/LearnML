(* C:\Users\jN\Documents\수업\프로그래밍언어\hw1_2.ml *)

exception Error of string

(* let matrix (x, y) =
  float_of_int x +. float_of_int y *)

let rec prod (matrix, i, k) =
  if k < 1 || i < 1 then raise (Error "invalid range")
  else if k = 1 then matrix (i, k)
  else matrix (i, k) *. prod (matrix, i, k-1)

let rec sumprod (matrix, n, k) =
  if n < 1 || k < 1 then raise (Error "invalid range")
  else if n = 1 then prod (matrix, n, k)
  else prod (matrix, n, k) +. sumprod (matrix, n-1, k)

(* let _ = print_float (sumprod (matrix, 1, 1));; *)