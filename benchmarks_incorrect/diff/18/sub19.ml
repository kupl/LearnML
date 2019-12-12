type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->  match exp with
  |Const a -> Const 0
  |Var s -> if s = x then Const 1 else Var s
  |Power (s, i) -> 
    if s = x then Times[Const i;Power (s, i - 1)] else Power (s, i)
  |Times l -> begin
  match l with
    |h1::h2::h3::[]->Sum[Times[diff (h1, x);h2;h3];diff (h2, x);diff(h3,x)]
    |h1::h2::[] -> Sum[Times[diff (h1, x);h2];diff (h2, x)]
    |h1::[]->Sum[Times[diff (h1, x)]]
    
    end
  |Sum m ->begin
  match m with
    |h1::h2::h3::h4::t -> Sum[diff(h1, x); diff(h2, x);diff(h3,x);diff(h4,x)]
    |h1::h2::h3::t -> Sum[diff(h1, x); diff(h2, x);diff(h3,x)]
    |h1::h2::t -> Sum[diff(h1, x); diff(h2, x)] 
    |h1::t -> Sum[diff(h1, x)]
    |[]->Sum[]
    end;;
  
  diff(Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1],"x");;