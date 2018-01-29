
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
 
let rec diff : aexp * string -> aexp
=fun (exp,var) ->

match exp with
Const (a) -> Const 0 |
Var (s) -> if s=var then Const 1 else Const 0 |
Power (s,a) -> if a=1 then Const 1 else Times [Const a; Power (s,a-1)] |
Times lst -> (match lst with [] -> Const 0 |
  h::t -> if t = [] then Sum [Times [(diff (h,var)); Const 1]; Times [h;(diff (Times t,var))]]
  else Sum [Times [(diff (h,var)); Times t]; Times [h;(diff (Times t,var))]]) |
Sum lst2 -> match lst2 with [] -> Const 0 | h::t -> Sum [(diff (h,var));(diff (Sum t,var))]