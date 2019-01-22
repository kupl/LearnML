
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
    match exp with
    | Const i -> Const 0
    | Var x ->
      if x = var then Const 1
      else Var x
    | Power(x,i) ->
      if x = var then Times[Const(i);Power(x,i-1)]
      else Power(x,i)
    | Sum(hd::tl) -> 
      begin
        match tl with
         | [] -> Sum[diff(hd,var)]
         | _ -> concat (Sum[diff(hd,var)]) (diff(Sum(tl),var))
      end
    | Times l -> asdf (l,var) 0
  and asdf (l,var) i =
    if i = ((lenght l) - 1) then Sum[diffTimes (l,var) 0 i]
    else concat (Sum[diffTimes (l,var) 0 i]) (asdf (l,var) (i + 1))
  and diffTimes ((hd::tl),var) now_i target_i = 
    if now_i = target_i then Times(diff(hd,var)::tl)
    else concat (Times[hd]) (diffTimes (tl,var) (now_i + 1) target_i)
  and lenght l =
    match l with
    | [] -> 0
    | hd::tl -> 1 + lenght tl
  and concat s1 s2 = 
    match s1 with
    | Sum l1 -> 
      begin
        match s2 with
        | Sum l2 -> Sum(l1@l2)
      end
    | Times l1 ->
      begin
        match s2 with
        | Times l2 -> Times(l1@l2)
      end