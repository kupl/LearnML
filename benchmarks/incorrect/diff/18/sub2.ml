type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (exp, x) -> 
  match exp with
    |Const a -> Const 0
    |Power (a, b) -> if a = x then Times [Const b; Power(a, b - 1)] else Power (a, b)
    |Var a -> if a = x then Const 1 else Var a
    |Sum lst ->
      let rec loop lst =
        match lst with
          |[] -> []
          |hd::tl -> [diff (hd, x)] @ (loop tl)
      in Sum (loop lst)
    |Times lst ->
      let rec loop lst =
        match lst with
          |[] -> []
          |hd::tl -> 
            [(
               match hd with
                |Times lst -> diff (hd, x)
                |Sum lst -> diff (hd, x)
                |Const a -> Const a
                |Var a -> diff (hd, x)
                |Power (a,b) -> diff (hd, x))] @ (loop tl)
      in Times (loop lst);;