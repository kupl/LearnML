open Lang
open Util
open Type

exception NotImplemented

module A = struct
	
	(* summary of function body *)
	type summary = 
        | E
		| F of pat list	(* Flat match *)
		| N of (pat * summary) list (* Nested match *)

	(* Function = (function name, input types, output type, summary) *)
	type t = (string * typ list * typ * summary) list

	(* To string *)
	let rec string_of_summary : summary -> string
	= fun summary ->
		"match {" ^
		match summary with 
        | E -> "Empty"
		| F ps -> List.fold_left (fun acc p -> acc ^ Print.pat_to_string p ^ " | ") "" ps
		| N summaries -> List.fold_left (fun acc (p, s) -> acc ^ Print.pat_to_string p ^ ":" ^ string_of_summary s ^ " | ") "" summaries
		^ " }"

	let string_of_t : t -> string
	= fun t ->
		List.fold_left (fun acc (name, inputs, output, summary) -> 
			acc ^ "(\n" ^
			"Name : " ^ name ^ "\n" ^
            "Input types : " ^ List.fold_left (fun acc typ -> acc ^ Print.type_to_string typ ^ ", ") "{" inputs ^ "}\n" ^
			"Output type : " ^ Print.type_to_string output ^ "\n" ^
			"Structure : \n" ^ string_of_summary summary ^ "\n)"
		) "" t

    let lookup_type : id -> TEnv.t -> typ
    = fun func_id tenv -> TEnv.find tenv func_id
      
    let rec explore_bind : binding list -> binding -> binding list
    = fun acc b ->
      let (_,_,_,_,e) = b in 
      explore_exp (b::acc) e
    
    and explore_exp : binding list -> lexp -> binding list
    = fun acc (_,exp) ->
      match exp with 
      | EApp (e1,e2) -> explore_exp [] e1 @ explore_exp [] e2 @ acc
      | ELet (bind,b,arg,typ,e1,e2) -> explore_bind acc (bind,b,arg,typ,e1)
      | EBlock (_,bs,_) -> List.fold_left explore_bind [] bs 
      | IF (_,e1,e2) -> explore_exp [] e1 @ explore_exp [] e2 @ acc
      | _ -> acc

    let explore_decl : binding list -> decl -> binding list 
    = fun acc decl->
      match decl with
      | DLet b -> (explore_bind [] b) @ acc
      | DBlock (_, bs) -> (List.fold_left explore_bind [] bs) @ acc
      | _ -> acc

    let explore_prog : prog -> binding list
    = fun decls -> List.fold_left explore_decl [] decls
    
    let extract_summary : binding -> (string * summary)
    = fun (f,_,_,_,e) -> 
      let rec get_pattern: lexp -> summary 
      = fun (_,exp) -> 
        match exp with
        | EApp (e1,e2) -> ignore (get_pattern e1); get_pattern e2
        | EFun (_,e) -> get_pattern e
        | EMatch (_,br) -> let pat = List.map (fun (x,y) -> x) br in F (pat)
        | IF(_,e1,e2) -> ignore (get_pattern e1); get_pattern e2;
        | _ -> E
      in 
      let name = Print.let_to_string f in
      let summary = get_pattern e in
      (name, summary)

  (* 
    Extract input and output types 
    ex) (t1 * t2) -> (t3 -> t4) => [t1; t2; t3] * t4
  *)
  let rec extract_type : typ -> (typ list * typ)
  = fun typ ->
    match typ with
    | TArr (t1, t2) ->
      let ts1 = type_of_input t1 in
      let (ts2, t) = extract_type t2 in
      (ts1@ts2, t)
    | t -> ([], t)

  and type_of_input : typ -> typ list
  = fun typ ->
    match typ with
    | TTuple ts -> List.fold_left (fun ts t -> (type_of_input t)@ts) [] ts
    | t -> [t]

  (* Summarize a given program *)
	let run : prog -> t
	= fun pgm -> 
      let (typ_env,_,_,_) = run pgm in
      let func_list = explore_prog pgm in
      let processing = List.map extract_summary func_list in
      let summaries = List.map (fun (x,z) -> 
        let (inputs, output) = extract_type (lookup_type x typ_env) in
        (x, inputs, output, z)
      ) processing 
      in
      summaries 
end

let rec remove_elem : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
= fun f e l -> 
  match l with 
  |[] -> []
  |x::xs -> if (f e x) then xs else x :: remove_elem f e xs

(*naive equivalence checking*)
let rec list_equivalence : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
= fun f s1 s2 -> 
  match s1,s2 with 
  | [],[] -> true 
  | [],_  
  |  _,[] -> false
  | x::xs, y::ys -> 
    let y'= (remove_elem f x (y::ys)) in
    list_equivalence f xs y'

let match_type : (typ list * typ) -> (typ list * typ) -> bool
= fun (ts, t) (ts', t') ->
  let rec range : int -> int -> int list
  = fun s e ->
    if s = e then [e]
    else if s < e then s::(range (s+1) e)
    else raise (Failure "Invalid range")
  in
  let rec insert_nth : 'a list -> 'a -> int -> 'a list
  = fun lst e n ->
    match lst with
    | [] -> if n = 0 then [e] else raise (Failure "Insert in invalid index")
    | hd::tl -> if n = 0 then e::lst else hd::(insert_nth tl e (n-1))
  in
  let rec permutation : 'a list -> ('a list) list
  = fun lst ->
    match lst with
    | [] -> []
    | [hd] -> [[hd]]
    | hd::tl -> List.fold_left (fun acc lst -> 
      let indices = range 0 (List.length lst) in
      let results = List.fold_left (fun acc n -> (insert_nth lst hd n)::acc) [] indices in
      List.fold_left (fun acc lst -> lst::acc) acc results  
    ) [] (permutation tl)
  in
  if List.length ts = List.length ts' then
    let ts = t::ts in
    let comp_candidate = List.map (fun ts' -> t'::ts') (permutation ts') in
    List.exists (fun ts' -> 
      let eqns = List.fold_left2 (fun eqns t t' -> (t, t')::eqns) [] ts ts' in
      try
        let _ = List.fold_left (fun subst (t1, t2) -> unify subst ((Subst.apply t1 subst), Subst.apply t2 subst)) Subst.empty eqns in
        print_endline ("Type Eqns : ");
        List.iter (fun (t1, t2) -> print_endline (Print.type_to_string t1 ^ " = " ^ Print.type_to_string t2)) eqns;
        true
      with TypeError -> false
    ) comp_candidate 
  else false

let rec match_pattern
= fun x y -> 
  let rec is_match : pat -> pat -> bool
  = fun p1 p2 ->
    match p1,p2 with
    | PUnit,PUnit -> true
    | PUnder, PUnder -> true
    | PInt _, PInt _ -> true
    | PBool b1, PBool b2 -> b1=b2
    | PVar _ , PVar _ -> true
    | PList l1, PList l2  
    | PCons l1, PCons l2 
    | PTuple l1, PTuple l2
    | Pats l1, Pats l2 -> list_equivalence is_match l1 l2
    | PCtor (id1,l1), PCtor (id2,l2) -> if id1=id2 then list_equivalence is_match l1 l2 else false 
    | _,_ -> false
  in
  match x,y with
  | A.E, A.E -> true
  | A.F l1, A.F l2 -> 
    let sorted_l1 = List.sort compare l1 in
    let sorted_l2 = List.sort compare l2 in
    list_equivalence is_match sorted_l1 sorted_l2  
  | A.N _ , A.N _ -> raise NotImplemented
  | _,_ -> false 

let match_summary 
= fun (f,ts,t,s) (f',ts',t',s') ->
  let pat_match = match_pattern s s' in
  let typ_match = match_type (ts,t) (ts',t') in
  typ_match && pat_match

let match_program : A.t -> A.t -> bool
= fun x y -> 
  list_equivalence match_summary x y

(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> prog list -> prog
= fun pgm cpgms -> 
	let cpgm = List.hd cpgms in
	cpgm

let get_summary : prog -> A.t
= fun pgm -> 
	let summary = A.run pgm in
	summary
