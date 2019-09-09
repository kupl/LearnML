type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;
  
let rec map f l k  =
  match l with
    | [] -> []
    | hd::tl -> (f (hd, k))::(map f tl k);;
  
let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with 
  | Const a -> Const 0
  | Sum lst -> Sum (map diff lst x)
  | Var var -> if var = x then Const 1 else Const 0
  | Power (var,exponent) -> if var = x then Times [Const exponent ; Power(var,exponent-1)] else Const 0
  | Times lst -> match lst with 
    | [] -> Const 0
    | [a] -> diff(a,x)
    | hd::tl ->  Sum[ Times [diff (hd, x); Times tl] ; Times [hd ; diff ((Times tl), x)] ];;
    
  
 
diff(Sum [Power ("x",2);Times [Const 2;Var "x"];Const 1],"x");;