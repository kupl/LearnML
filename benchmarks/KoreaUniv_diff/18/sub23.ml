type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> exp = match exp with
| Const i -> i
| Var string -> string
| Power of string * int -> string^int
| Times of aexp list -> match list with
    |[]->[]
    |hd::tl->hd "*" Times of aexp (tl)
| Sum of aexp list -> match lst with
      |[]->[]
      |hd::tl->hd "+" Sum of aexp (tl);;



Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
