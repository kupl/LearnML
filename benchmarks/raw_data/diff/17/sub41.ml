(* problem 4 *)
type aexp = 
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list
let rec diff : aexp * string -> aexp
= fun (e, x) -> match e with
| Const n -> Const 0
| Var y -> if y = x then Const 1 else Const 0
| Power (x, 0) -> Const 0
| Power (y, n) -> if y = x then
                  Times [Const n; Power (x, n-1)] else Const 0
|Times [Const n; Var y] -> if y = x then Const n else Const 0
| Times [Const k; Power (y, n)] -> if y = x then
  Times [Const k; Const n; Power (x, n-1)] else Const 0
| Times [Power (y, p); Power (x, n)] -> if y = x then
  Times [Const (p+n); Power (x, n+p-1)] else
  Times [Const n; Power (y, p); Power (x, n-1)]
| Sum [Const n; Var y] -> if y = x then Const 1 else Const 0
| Sum [Const k; Power (y, n) ] -> if y = x then
  Times [Const n; Power (x, n-1)] else Const 0;;