type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
let rec calculate : aexp -> aexp
= fun exp -> match exp with
  |
  | Times(lst) -> match lst with
    | [] -> Times([])
    | h::t -> h

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  | Const int -> Const 0
  | Var str -> if (x = str) then Const 1
               else Var str
  | Power(str, int) -> if (x = str) then Times[Const int; Power(str, int-1)] 
                       else Power(str, int)
  | Times(lst) -> match lst with
    | [] -> Const 0
    | h::t -> 
  | Sum(lst) -> match lst with
    | [] -> Const 0
    | h::t -> [diff(h, x)]@diff(Sum(t));;
