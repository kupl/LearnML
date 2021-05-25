(* problem 4*) 

  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e, v) ->
      match e with
      | Const a -> Const 0
      | Var s   -> if s=v then Const 1 else Const 0
      | Power (s,n) -> if s=v then Times[Const n; Power(s,n-1)] else Const 0     
      | Times lst -> (match lst with
                    | []    -> Const 1   
                    | h::[] -> diff(h,v)            
                    | h::t  -> (match h with
                                | Const 0 -> Const 0
                                | Const 1 -> diff (Times t,v)
                                | Const n -> Times[Const n; diff (Times t,v)]
                                | _ -> Sum [Times(diff(h,v)::t); Times[h;diff(Times t,v)]]))
      | Sum lst2 -> (match lst2 with
                    | []    -> Const 0
                    | h::[] -> diff(h,v)
                    | h::t  -> Sum [ (diff(h,v)) ; (diff(Sum t,v)) ])