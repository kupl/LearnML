
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
| Const _ -> Const 0
| Var y -> if var = y then Const 1 else Const 0
| Power (y,n) -> if not (var = y) then Const 0
									else Times [Const n; Power (y,n-1)]
| Times [] -> Const 0
| Times (hd::tl) -> Sum [Times (diff (hd,var)::tl); Times [hd; diff(Times tl, var)]]
| Sum aexp_bunch -> Sum (List.map (fun ae_temp -> diff(ae_temp,var)) aexp_bunch);;
