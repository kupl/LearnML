(* problem 7*)
type digit = ZERO | ONE
type bin = digit list


let rec exponent b =
if (b == 0) then 1
else 2 * exponent (b-1)

let rec decimal b1 = 
match b1 with
|[]->0
|h::t-> if h == ONE then (exponent (List.length t)) + decimal t else 0 + decimal t

let rec element x =
match x with
|1-> ONE 
|0-> ZERO
|_->raise (NOANSWER)

let rec binary n l =
match n with
|0->l
|_-> binary (n/2) ((element(n mod 2))::l)

let sum b1 b2 = binary (decimal b1 * decimal b2) []


let bmul: bin -> bin -> bin
= fun b1 b2 -> sum b1 b2

