(*
type nat = ZERO | SUCC of nat
*)

let rec nat2int n = match n with
	| ZERO -> 0
	| SUCC t -> 1+(nat2int t)

let rec int2nat i = match i with
	| 0 -> ZERO
	| _ -> SUCC (int2nat (i-1))

let natadd (n1, n2) = int2nat((nat2int n1) + (nat2int n2))

let natmul (n1, n2) = int2nat((nat2int n1) * (nat2int n2))

(*
let a = int2nat 10
let b = int2nat 13

let c = natadd(a,b)
let d = natmul(a,b)

let _ = print_endline(string_of_int (nat2int c))
let _ = print_endline(string_of_int (nat2int d))

let _ = print_endline(string_of_int (nat2int (natadd(c, d))))
let _ = print_endline(string_of_int (nat2int (natmul((int2nat 3), (int2nat 9)))))

