
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec simp : aexp -> aexp
  = fun (exp) ->
      match exp with
      Const n -> Const n
      | Var x -> Var x
      | Power (s,i) -> 
          (match i with 
            0 -> Const 1
            | 1 -> Var s
            | _ -> Power(s,i))
      | Times expli ->
          (match expli with
            [] -> Const 1
            | h::[] -> simp(h)
            | h::t -> match h with Const 0 -> Const 0
            					| Const 1 -> Times(t)
                                | _ -> Times(h:: t))
            (* h: aexp, t: aexp list *)
      | Sum (expli) ->
          (match expli with
            [] -> Const 0
            | h::[] -> simp(h)
            | h::t -> match h with Const 0 -> Sum t
                                    | _ -> Sum(h:: t))
              (* h: aexp, t: aexp list *)

 
  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
    Const n -> Const 0
    | Var x -> if x=var then Const 1 else Const 0
    | Power (s,n) ->
      if s=var then (match n with 0 -> Const 0
                                | 1 -> Const 1
                                | _ -> (Times [Const n; Power(s, n-1)]))
      else Const 0
    | Times li ->   (* li// aexp list *)
        (match li with [] -> Const 1
                       | h::[] -> diff(h, var)
                       | h::t -> (match h with Const n -> simp(Times [Const n; diff(Times t,var)])
                                              (* | _ -> (Sum [Times (diff(h,var)::t); Times [h; diff(Times t,var)]])) *)
                                              | _ -> simp(Sum [Times (diff(h,var)::t); Times [h; diff(Times t,var)]])))
    | Sum li ->
          (match li with
            [] -> Const 0
            (* | h::t -> (Sum [diff(h,var); diff(Sum t, var)])) *)
            | h::t -> simp(Sum [diff(h,var); diff(Sum t, var)]))