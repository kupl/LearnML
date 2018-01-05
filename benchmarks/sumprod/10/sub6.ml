(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-2 *)

exception Error of string

let rec sum (i, l) = if i < 1 then 0.0
  		else match l with [] -> raise (Error "invalid input")
  			| h::t -> sum (i-1, t) +. h

let rec mult (f, i, j) = if i < 1 then raise (Error "boundary error")
  			else if j < 1 then 1.0
  			else mult (f, i, j-1) *. f (i, j)

let rec prod (f, i, j) = if i < 1 then []
  		        else mult(f, i, j) :: prod(f, i-1, j)

let sumprod (matrix, n, k) = sum (n, prod (matrix, n, k))

(*
(* test code *)
let m (i, j) = float_of_int (i + 2 * j)

let _ = print_endline (string_of_float (sumprod(m, 3, 5)))
*)
