
 type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec hasVar
= fun (exp, var) ->
match exp with
  | hd::tl -> (match hd with
               | Const x -> false
               | Var x -> if x = var then true else false
               | Power (x, y) -> if x = var
                                 then (if y != 0 then true else false)
                                 else false
               | Times l -> hasVar (l, var)
               | Sum l -> hasVar (l, var)
              ) || hasVar(tl, var)
  | [] -> false;;

  let rec diff : aexp * string -> aexp
    = fun(exp, var) -> match exp with
      | Power (x, y) -> if x = var then
                          (if y = 0 then Const 0
                           else if y = 1 then Const y
                           else Times [Const y; Power(x, y-1)])
                        else (if y = 0 then Var x else Power(x,y))
      | Const x -> Const 0
      | Var x -> if x = var then Const 1 else Const 0                                                   
      | Sum l -> (match l with
                  | [] -> Const 0
                  | [x] -> diff (x, var)
                  | hd::tl -> Sum[diff (hd,var); diff(Sum tl,var)])
      | Times l -> let flag = hasVar(l,var) in
                   (match l with 
                    | [] -> Const 0
                    | [x] -> if flag = true then (* Var exists! *)
                             (if diff(x,var) = Const 0 then x else diff(x,var))
                             else Const 0
                    | hd::tl -> Times [(if flag = true then
                                         (if diff(hd,var) = Const 0
                                          then hd else diff(hd,var))
                                        else Const 0); diff(Times tl,var)]
                   );;