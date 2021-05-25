type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
  Const c -> Const 0
| Var v -> if v = x then Const 1 else Const 0
| Power (v, n) -> if v != x then Const 0 else Times [Const n; Power (v, n-1)]
| Times (th::tt) -> Sum [Times (diff(th, x)::tt); Times [th; diff(Times tt, x)]]
| Sum s -> Sum (List.map (fun aexp2 -> diff(aexp2, x)) s)
| Times [] -> Const 0;;
