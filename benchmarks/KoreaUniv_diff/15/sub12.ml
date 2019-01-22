type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
Const n -> Const 0
|Times [Const a ; Power (b,c)] -> if b=x then Times [Const (a*c); Power (b, (c-1))]
                                                       else Const 0
|Times [Const a; Var b] -> if b=x then Const a else Const 0
|Power (a, b) -> if a=x then Times [Const b; Power(a, (b-1))]
                            else Const 0
|Sum n -> Sum (sum n x) ;; 
