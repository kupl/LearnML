
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, var) ->
     match exp with
     | Const c -> Const 0
     | Var x -> if x=var then Const 1 else Const 0
     | Power (x, c) -> if x=var then 
                   (if c=1 then Const 1 else Times [Const c; Power (x, c-1)])
                    else Const 0
     | Times lst -> (match lst with
                     | [] -> Const 1
                     | hd::tl -> if tl=[] then diff (hd, var) 
      else Sum [Times[(diff (hd, var)); Times tl]; Times[hd; (diff (Times tl, var))]])
     | Sum lst -> (match lst with
                   | [] -> Const 0
                   | hd::tl -> if tl=[] then diff (hd, var) 
      else Sum [(diff(hd, var)); (diff(Sum tl, var))]);;