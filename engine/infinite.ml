open Lang
open Util

(* If the given program has infinite loop, return true *)

module Static = struct

(*********************************************)
(* if every branches execute recursive calls *)
(*********************************************)	

	let name_of_func : let_bind -> id
	= fun f ->
		match f with
		| BindOne f -> f
		| _ -> raise (Failure "Only variables are allowed as left-hand side of `let rec'")

	let rec is_arg : id -> arg -> bool
	= fun f arg ->
		match arg with
  	| ArgOne (x, _) when x = f -> true
 		| ArgTuple xs -> List.exists (is_arg f) xs
 		| _ -> false

	let rec is_bind : id -> let_bind -> bool
	= fun f x ->
		match x with
  	| BindOne x when x = f -> true
 		| BindTuple xs -> List.exists (is_bind f) xs
 		| _ -> false

	let rec check_exp : id -> lexp -> bool
	= fun f (_, exp) ->
		match exp with
		| EList es | ETuple es | ECtor (_, es) -> List.exists (check_exp f) es
    | MINUS e | NOT e | Raise e -> check_exp f e
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | OR (e1, e2) | AND (e1, e2) 
    | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) 
    | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) -> (check_exp f e1) || (check_exp f e2)
    | EApp (e1, e2) -> 
    	begin match snd e1 with
    	| EVar x when x = f -> true
    	| _ -> check_exp f e1 || check_exp f e2
    	end
    | EFun (arg, e) -> if is_arg f arg then false else check_exp f e
    | ELet (f', is_rec, args, typ , e1, e2) -> 
    	if is_bind f f' then
    		check_exp (name_of_func f') e1
    	else
    		check_exp (name_of_func f') e1 || check_exp f e2
    | EBlock (is_rec, bindings, e2) ->
    	if (List.exists (fun (f', is_rec, args, typ, e) -> is_bind f f') bindings) then
    		(List.exists (fun (f', is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f') e else false) bindings)
    	else
    		(List.exists (fun (f', is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f') e else false) bindings) || check_exp f e2
    | EMatch (e, bs) ->
    	let (_, es) = List.split bs in
    	if (check_exp f e) then true else List.for_all (check_exp f) es
    | IF (e1, e2, e3) -> if (check_exp f e1) then true else (check_exp f e2) && (check_exp f e3)
    | _ -> false

	let rec check_decl : decl -> bool
	= fun decl ->
		match decl with
    | DLet (f, is_rec, args, typ, e) -> if is_rec then check_exp (name_of_func f) e else false 
    | DBlock (is_rec, bindings) -> List.exists (fun binding -> check_decl (DLet binding)) bindings
    | _ -> false

(*******************************************)
(* Recursive call with unchanged arguments *)
(*******************************************)

	let rec argequiv: arg -> arg -> bool
	= fun arg1 arg2 ->
		match arg1,arg2 with
		| ArgOne (x1,_), ArgOne (x2,_) -> x1=x2
		| ArgTuple l1, ArgTuple l2 -> List.for_all2 argequiv l1 l2
		|_ -> false

	let rec exp2arg : lexp -> arg
	= fun (_, exp) ->
		match exp with
		| ETuple l -> ArgTuple (List.map exp2arg l)
		| EVar x -> ArgOne (x,TUnit)
		| _ -> raise (Failure "translating exp failed")

	let rec matching_args : lexp -> (id * arg list) -> bool
	= fun (_, exp) (f,args) ->
		begin match exp with
			| EVar x -> if(args=[]) then x=f else false
			| EApp (e1,e2) ->
				begin try 
					let arg_hd = List.hd args in
					let earg = exp2arg e2 in
					if(argequiv earg arg_hd) then matching_args e1 (f,List.tl args)
					else false
				with
				|_ -> false
		    end
      |_ -> false
    end

  let rec arg2id : arg -> id list
  = fun arg -> 
    match arg with
    | ArgOne (x,_) -> [x]
    | ArgTuple l1 -> list_fold (fun arg l -> (arg2id arg)@l) l1 []
    | _ -> []

  let rec func2id : (id*arg list) -> id list
  = fun (f,args) ->
    f::(list_fold (fun arg l -> (arg2id arg) @ l) args [])  
  
  let rec pat2id : pat -> id list
  = fun pat ->
    match pat with
    | PVar x -> [x]
    | PList l | PTuple l | PCtor (_,l) | PCons l -> list_fold (fun p l -> (pat2id p)@l) l [] 
    | _ -> []

  let rec check_exist : id list -> id list -> bool
  = fun new_one old_one ->
    match new_one with
    |[] -> false
    |hd::tl -> List.mem hd old_one || check_exist tl old_one

  let rec is_arg2 : arg -> (id * arg list) -> bool
  = fun arg func ->
    let new_lst = arg2id arg in
    let old_lst = func2id func in
    check_exist new_lst old_lst


  let rec is_pat : pat -> (id * arg list) -> bool
  = fun p func ->
    let new_lst = pat2id p in
    let old_lst = func2id func in
    check_exist new_lst old_lst

  let bind2id : let_bind -> id
  = fun f ->
    match f with
    | BindOne x -> x
    | _ -> raise (Failure "bind failed")

  let rec preprocess :  lexp -> (lexp * arg list)
  = fun e ->
    match snd e with
    | EFun (arg,e) -> 
      let (e,arg_list) = preprocess e in
      (e,arg::arg_list)
    | _ -> (e,[])

	let rec check_exp2 : ((id * arg list)) -> lexp -> bool
	= fun func (l, exp) ->
		match exp with
		| EList es | ETuple es | ECtor (_, es) -> List.exists (check_exp2 func) es
    | MINUS e | NOT e | Raise e -> check_exp2 func e
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | OR (e1, e2) | AND (e1, e2) 
    | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2) | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) 
    | AT (e1, e2) | DOUBLECOLON (e1, e2) | STRCON (e1, e2) -> (check_exp2 func e1) || (check_exp2 func e2)
    | EApp (e1, e2) -> 
      matching_args (l,exp) func || check_exp2 func e1 || check_exp2 func e2 (* up to here *)
    | EFun (arg, e) -> if is_arg2 arg func then false else check_exp2 func e
    | ELet (f', is_rec, args', typ , e1, e2) -> 
      if is_rec then 
        check_exp2 (bind2id f',List.rev args') e1 || check_exp2 func e2
      else
        check_exp2 func e2
    | EBlock (is_rec, bindings, e2) ->
      if is_rec then
        (List.exists (fun (f,_,args,_,e) -> check_exp2 (bind2id f,List.rev args) e) bindings) || check_exp2 func e2
      else 
        (List.exists (fun (_,_,_,_,e)-> check_exp2 func e) bindings) || check_exp2 func e2
    | EMatch (e, bs) ->
    	(check_exp2 func e) || List.exists (fun (p,e) ->
        if (is_pat p func) then false
        else check_exp2 func e
      ) bs
    | IF (e1, e2, e3) -> (check_exp2 func e1) || (check_exp2 func e2) || (check_exp2 func e3)
    | _ -> false

 	let rec check_decl2 : decl -> bool
	= fun decl ->
		match decl with
		| DLet (f, is_rec, args, typ, e) -> 
      begin try
        if is_rec then 
          let (e,arg_list) = preprocess e in
					let f = bind2id f in
          check_exp2 (f,List.rev(args@arg_list)) e 
        else false
		  with
      |_ -> false
      end
    | DBlock (is_rec,bindings) ->
      if is_rec then
        List.exists (fun (f,_,args,_,e) -> 
          let (e,arg_list) = preprocess e in
          check_exp2 (bind2id f,List.rev(args@arg_list)) e
        ) bindings
      else
        false
    | _ -> false
	let run : prog -> bool
	= fun pgm -> 
		(List.exists (check_decl) pgm) || (List.exists (check_decl2) pgm)

end
