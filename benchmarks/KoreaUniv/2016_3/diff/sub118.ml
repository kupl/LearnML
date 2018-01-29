
  type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  =fun (aexp,x) -> match aexp with
    |Const a -> Const 0
    |Var "x" -> if "x" = x then Const 1 else Const 0
    |Power ("x", a) -> (match a with
      |2 -> Times[Const 2; Var "x"]
      |1 -> Const 1
      |0 -> Const 0
      |_ -> Times[Const a; Power ("x", a-1)])
    |Times l -> (match l with
      | [] -> Const 0
      | [a] -> diff (a,x)
      | h::t -> Sum[Times[diff (h,x);Times t]; Times[h;diff (Times t,x)]])
    |Sum ll -> (match ll with
      | [] -> Const 0
      | [a] -> diff (a,x)
      | h::t -> Sum [diff (h,x); diff (Sum t,x)])