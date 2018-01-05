(* Ex4 *)
let rec prod(matrix, (n:int), k) =
   if k < 1 then 1.
   else matrix(n, k) *. prod(matrix, n, k-1)