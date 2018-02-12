type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
|Const a -> Const 0
|Var "x" -> Const 1
|Power ("x", a) -> (match a with
 |2 -> Times[Const 2; Var "x"]
 |1 -> Const 1
 |0 -> Const 0
 |_ -> Times[Const a; Power ("x", a-1)])