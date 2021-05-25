
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
      Const c -> Const 0
    | Var s -> if s = var then Const 1 else Const 0
    | Power (s, n) -> begin
		                  if s <> var then Const 0
                      else match n with
                        1 -> Times [Const 1]
                      | _ -> Times [Const n; Power (s, n-1)]
                      end
    | Times l -> begin
                 match l with
                 | hd::tl -> Sum [Times (diff (hd, var)::tl); Times [hd; diff (Times tl, var)]]
                 end