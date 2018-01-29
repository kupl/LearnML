
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> (* TODO *)
    match exp with
    | Const(a) -> Const(0)
    | Var(a) ->
        if a=var then Const 1
        else Const(0)
    | Power(a,b) ->
        if a=var then (
          if b = 1 then Times([Const b; Var a])
          else if b = 0 then Const 0
          else Times([Const b; Power( a , b-1 )])
        )
        else Const(0)
    | Times(li) ->
        (
          match li with
          | [] -> Times([])
          | hd::tl ->
              if tl = [] then diff (hd,var)
              else if hd = Const 0 then Const 0
              else
                let h = diff (hd,var) 
                  in let t = diff ((Times tl),var)
                    in (
                      if h = Const 0 then Times ([hd ; t])
                      else if t = Const 0 then Times (h::tl)
                      else
                        Sum ([Times (h::tl) ; Times ([hd ; t])])
                   )
        )

    | Sum(li) ->
        (
          match li with
          | [] -> Sum([])
          | hd::tl ->
              if tl = [] then diff (hd,var)
              else Sum( [ diff (hd,var) ; diff ((Sum tl),var) ] ) 
        )