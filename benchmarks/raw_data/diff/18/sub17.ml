type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
  | Const n -> Const 0
  | Var v -> if (v <> x) then Const 0 else Const 1
  | Power (v, p) -> 
    if (v <> x) then Const 0 
    else Times [Const p; Power (v, p-1)]
  | Times li ->
    (match li with
    | [] -> Const 0
    | hd::tl -> (* (f * g)` = f` * g + f * g` *) 
      Sum [Times ((diff (hd, x))::tl); Times [hd; diff (Times tl, x)]])
  | Sum li -> 
    (match li with
    | [] -> Const 0
    | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)]);;
    
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
      
  