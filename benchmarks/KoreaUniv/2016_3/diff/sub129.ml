
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> (* TODO *)
  
    match exp with
    Const sth -> Const 0
    |Var sth -> if (sth = var) then (Const 1) else (Const 0)
    |Power (sth, n) -> if (sth <> var) then (Const 0)
                       else Times [Const n; Power (sth, (n-1))] 
    |Times sth ->
			(
      match sth with
      [] -> Const 0
      |hd::tl -> Sum [ Times ( (diff (hd, var))::tl ) ;
								 Times [ hd ; diff ((Times tl), var) ] ]
   		)
    |Sum sth ->
       let rec map f l =
       (
       match l with
       [] -> []
       |hd::tl -> (f hd)::(map f tl)
       )
       in
			 (
       match sth with
       [] -> Const 0
       |_ -> Sum ( map (fun element -> diff (element, var)) sth )
       )  