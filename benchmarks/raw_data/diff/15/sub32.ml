type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
    | Const (n) -> Const 0
    | Var (str) -> if (str = x) then Const 1 else Const 0 
    | Power (str, n) -> if (str = x) then Times [Const (n); Power (x, n-1)] else Const 0
    | Times (l) -> (match l with
                | [] -> Const 0
                | hd :: tl -> if (hd = Const 0) then Const 0
                              else Sum ([Times ([diff (hd,x)] @ tl)] @ [Times([hd] @ [diff (Times (tl),x)])]))
    | Sum (l)-> (match l with
              | [] -> Const 0
              | hd :: tl -> if (hd = Const 0) then Sum (tl)
                            else Sum([diff (hd,x) ]@[Sum([diff (Sum (tl),x) ])]));;


