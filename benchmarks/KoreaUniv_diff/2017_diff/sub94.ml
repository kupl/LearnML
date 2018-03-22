(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> Var(n)
ev(Const(n)) = Const(n)
ev(Plus(e1,e2)) = (cas ev(e1) of
 Var(n) => Plus(Var(n),ev(e2))  |
 Const(n) => (case ev(e2) of
   Var(m) => Plus(Const(n),Var(m))
   Const(m) => Const(n+m)
   Plus(e3,e4) => Plus(Const(n),Plus(e3,e4)) |
 Plus(e3,e4) => Plus(Plus(e3,e4),ev(e2)) );
