
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> (* raise NotImplemented (* TODO *) *)
      match exp with
      | Const a -> Const 0
      | Var s   -> if s=var then Const 1 else Const 0
      | Power (s,n) -> if s=var then Times[Const n; Power(s,n-1)] else Const 0     
      | Times lst -> (match lst with
                    | []    -> Const 1   
                    | h::[] -> diff(h,var)            
                    | h::t  -> (match h with
                                | Const 0 -> Const 0
                                | Const 1 -> diff (Times t,var)
                                | Const n -> Times[Const n; diff (Times t,var)]
                                | _ -> Sum [Times(diff(h,var)::t); Times[h;diff(Times t,var)]]))
      | Sum lst2 -> (match lst2 with
                    | []    -> Const 0
                    | h::[] -> diff(h,var)
                    | h::t  -> Sum [ (diff(h,var)) ; (diff(Sum t,var)) ])