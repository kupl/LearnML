type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
  match aexp with
  | Const n -> Const 0 
  | Var chars -> Var chars
  | Power(chars, n) -> if n>2 then Times [Const n; Power(chars, n-1)] else Times [Const n; Var chars]
  | Times lst ->
    begin match lst with
    | [] -> Const 0
    | aexp::tl ->
      (match aexp with
       | Const n -> Const n )
    end
  | Sum lst ->
    begin match lst with
    | [] -> Const 0
    | hd ->  Const 0
    | hd::tl -> Sum [diff(hd,x); (Sum tl)]
    end
