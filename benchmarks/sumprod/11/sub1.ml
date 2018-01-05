(* 2004-11951 Noh, Soon Hyun *)

(* Professor said that Ocaml is a beautiful language that has no variable,
but I had to use cnt acting as fake variable *)

(* product of one row *)
(* cnt is counter *)
let rec partialprod(matrix, i, k, cnt) =
	if cnt=k then matrix(i, cnt)
	else matrix(i, cnt) *. partialprod(matrix, i, k, cnt+1)

(* middle-step function of sumprod which has cnt argument *)
let rec protosumprod(matrix, n, k, cnt) =
	if cnt=n then partialprod(matrix, cnt, k, 1)
	else partialprod(matrix, cnt, k, 1) 
	+. protosumprod(matrix, n, k, cnt+1)
	(* divided one line to two lines *)

let sumprod(matrix, n, k) = protosumprod(matrix, n, k, 1)

(* Test Code ::
let mtx1(a, b) =
	match (a, b) with
	| (1, 1) -> 1.
	| (1, 2) -> 2.
	| (1, 3) -> (-2.)
	| (2, 1) -> 3.
	| (2, 2) -> 4.
	| (2, 3) -> 2.
	| _ -> 1000. (* for detection *)

let _ = print_float (sumprod(mtx1, 2, 3)); print_char '\n'
*)
