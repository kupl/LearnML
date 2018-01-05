(* 2004-11951 Noh, Soon Hyun *)

(* Skeleton type *)
(* I think (ZERO) is zero, and (SUCC (SUCC (SUCC ZERO))) is three.
Is this what this function means? **)
type nat = ZERO | SUCC of nat

(* adding two num *)
let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	(* a+b = 1+((a-1)+b) *)
	| (SUCC x, y) -> (SUCC (natadd (x, y)))
	| (x, (SUCC y)) -> (SUCC (natadd (x, y)))

(* multiplying two num *)
let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, ZERO) -> ZERO
	(* a*b = ((a-1)*b) + (1*b) *)
	| (SUCC x, y) -> (natadd ((natmul (x, y)), y))
	| (x, (SUCC y)) -> (natadd (x, (natmul (x, y))))

(* Print function and Test Code ::
let rec natprint a =
	match a with
	| ZERO -> 0
	| (SUCC x) -> ((natprint x) + 1)

let three = (SUCC (SUCC (SUCC ZERO)))
let four = (SUCC (SUCC (SUCC (SUCC ZERO))))
let seven = (natadd (three, four))
let _ = print_int (natprint(three)); print_char '\n'
let _ = print_int (natprint(four)); print_char '\n'
let _ = print_int (natprint ((natmul (seven, (natmul(seven, four)))))); print_char '\n'
*)
