
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
    | Const n -> Const 0
    | Var n -> if n=var then Const 1
                else Const 0
    | Power(a,b) ->if a=var && b!=0 then Times([Const b; Power( a , b-1 )])
                   else Const 0
    | Times n ->( match n with
          | [] -> Times([])
          | hd::tl ->if tl = [] then diff (hd,var)
                    else Sum ([Times (diff (hd,var)::tl) ; Times ([hd ; diff ((Times tl),var)])])
        )

    | Sum n ->( match n with
          | [] -> Sum([])
          | hd::tl -> if tl = [] then diff (hd,var)
                      else Sum( [ diff (hd,var) ; diff ((Sum tl),var) ] ) 
        )