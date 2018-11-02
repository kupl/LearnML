(* problem 3*)
type formula = 
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
           let rec lst_env e lst = (match lst with
                                    | hd::tl -> if tl = [] then[e::hd] else (e::hd)::lst_env e tl
                                | []->[e]::[]
               )
           in let rec case_env e = (match e with
                                | hd::tl -> (lst_env (hd,true) (case_env tl)) @ (lst_env (hd,false) (case_env tl))
                                | [] -> []
               )
           in let rec apply_env x e =
           ( match e with
            | [] -> raise (Failure ("variable " ^ x ^ " not found"))
            | (y,v)::tl -> if x = y then v else apply_env x tl
           )
            in let rec mk_env exp = (match exp with
                            | True -> []
                            | False -> []
                            | Var x -> [x]
                            | Neg e -> mk_env e
                            | And(e1,e2) -> ((mk_env e1)@(mk_env e2))
                            | Or(e1,e2) -> (mk_env e1)@(mk_env  e2)
                            | Imply(e1,e2) -> (mk_env e1)@(mk_env e2)
                            | Iff(e1,e2) -> (mk_env e1)@(mk_env e2)
                )
            in let rec eval exp env = (match exp with
                            | True -> true
                            | False -> false
                            | Var x -> apply_env x env
                            | Neg(e) -> (match eval e env with
                                      | true -> false
                                      | false -> true)
                            | And(e1,e2) -> let v1 = eval e1 env 
                                            in let v2 = eval e2 env 
                                            in v1 && v2
                            | Or(e1,e2) -> let v1 = eval e1 env 
                                           in let v2 = eval e2 env 
                                           in v1 || v2
                            | Imply(e1,e2) -> let v1 = eval e1 env
                                              in let v2 = eval e2 env 
                                              in if v1 then v2 else true
                            | Iff(e1,e2) -> let v1 = eval e1 env 
                                            in let v2 = eval e2 env 
                                            in if v1=v2 then true else false
                            )
          in let rec chk_envlst func elst = ( match elst with 
                            |hd::tl ->
                                if tl = [] then eval func hd 
                                else
                                if (chk_envlst func tl) then true else (eval func hd)
                          
                            |[] -> eval f []
                            )
         in chk_envlst f (case_env (mk_env f))