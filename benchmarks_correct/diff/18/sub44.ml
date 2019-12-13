type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    |Const _ -> Const 0
    |Var y -> if y = x then Const 1 else Const 0
    |Power (y,n) -> if y = x then Times[Const n; Power(y, n-1)] else Const 0
    |Times elist ->
      begin
      match elist with
        |[]-> Const 0
        |hd :: tl -> Sum [Times [diff(hd, x); Times tl]; Times [hd; diff(Times tl, x)]]
      end
    |Sum elist ->
      begin
      match elist with
        |[]-> Const 0
        |hd :: tl -> Sum[diff(hd,x); diff(Sum tl,x)]
      end;;
      
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
