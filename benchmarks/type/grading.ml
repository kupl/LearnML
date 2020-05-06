type grading_env = (tyvar * tyvar) list

let rec find : tyvar -> grading_env -> tyvar
= fun v env ->
  match env with
  |[] -> raise (Failure "Does not exist")
  |(hd,r)::tl -> if (hd=v) then r else find v tl

let extend : tyvar -> tyvar -> grading_env -> grading_env
= fun x y env -> (x,y)::env

let grading_num = ref (-1)

let fresh_grading_var _ = 
  let _ = grading_num := !grading_num + 1 in
  "t" ^ (string_of_int !grading_num)

let rec post_processing : typ -> grading_env -> (typ * grading_env)
= fun typ env ->
  match typ with
  | TyVar x -> 
    if List.exists (fun (y, typ) -> x = y) env then 
      (TyVar (find x env), env) 
    else 
      let fv = fresh_grading_var () in 
      (TyVar fv,extend x fv env)
  | TyFun (t1,t2) ->
    let (t1,env) = post_processing t1 env in
    let (t2,env) = post_processing t2 env in
    (TyFun (t1,t2),env)
  |_ -> (typ,env)

let grading : exp -> typ
= fun exp ->
	let ty = typeof exp in
	let (ty, _) = post_processing ty [] in
	ty