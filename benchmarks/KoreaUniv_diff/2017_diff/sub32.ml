(* problem 4*)
module Problem4 = struct
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, var) -> 
match exp with
| Consti -> Const 0
| Var i -> if i = var then Const 1 else Const 0
| Power(i, n) -> if i = var then Times[Const n; Power(i, n-1)]
else Const 0;
| Times [] -> Const 0
| Times (m::n) -> Sum[Times(diff(m, var)::n); Times[m; diff(Times n, var)]]
| Sum [] -> Const 0
| Sum (m::[]) -> diff(m, var)
| Sum (m::n) -> Sum[diff(m, var); diff(Sum n, var)]
end;;
