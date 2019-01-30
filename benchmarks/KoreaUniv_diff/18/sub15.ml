type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> (*TODO*)
  match exp with
    Const m -> Const 0
    |Var n -> if x = n then Const 1 else Const 0
    |Power (a,b) -> Times [Const b;diff(Var a,x);Power (a,b-1)]
    |Times l -> begin 
        match l with
          [a] -> diff(a,x)
          |hd::tl -> Sum[Times ((diff (hd,x))::tl);Times (hd::[diff (Times tl,x)])]
    end
     |Sum lst ->
        match lst with
        [] ->  Const 0
        |hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)];;
        
   