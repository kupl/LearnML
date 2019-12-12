
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
|Const n -> Const 0
|Var s -> if s=var then Const 1 else Const 0
|Power (s,n) -> if s=var then Times [Const n;Power(s,n-1)] else Const 0 

|Times t -> (match t with [] -> Const 0
												|hd::tl -> Sum[Times ([(diff (hd,var))]@tl); Times[hd;diff (Times tl,var)]])
|Sum l -> (match l with [] -> Const 0
                        |hd::tl -> Sum[diff(hd,var);diff(Sum tl,var)])