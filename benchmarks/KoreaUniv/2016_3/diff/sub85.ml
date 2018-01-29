
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> (* TODO *)
    match exp with
    | Const _ -> Const 0
    | Var k -> if var = k then Const 1 else Const 0
    | Power (k,m) ->  if not (var = k) then Const 0
                      else Times [Const m; Power (k, m-1)]
    | Times [] -> Const 0
    | Times (head::tail) -> Sum [Times (diff (head,var)::tail); Times [head; diff(Times tail, var)]]
    | Sum aexps -> Sum (List.map (fun dum -> diff (dum,var)) aexps);;