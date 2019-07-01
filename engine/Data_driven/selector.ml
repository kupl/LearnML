open Lang
open Util

exception NotImplemented

module A = struct
	
	(* summary of function body *)
	type summary = 
        | E
		| F of pat list	(* Flat match *)
		| N of (pat * summary) list (* Nested match *)

	(* Function = (function name, type, summary) *)
	type t = (string * typ * summary) list

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
		List.fold_left (fun acc (name, typ, summary) -> 
			acc ^ "(\n" ^
			"Name : " ^ name ^ "\n" ^
			"Type : " ^ Print.type_to_string typ ^ "\n" ^
			"Structure : \n" ^ string_of_summary summary ^ "\n)"
		) "" t

    let lookup_type : id -> Type.TEnv.t -> typ
    = fun func_id tenv -> Type.TEnv.find tenv func_id
      
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

    (* Summarize a given program *)
	let run : prog -> t
	= fun pgm -> 
      let (typ_env,_,_) = Type.run pgm in
      let func_list = explore_prog pgm in
      let processing = List.map extract_summary func_list in
      let summaries = List.map (fun (x,z) -> (x, (lookup_type x typ_env), z)) processing in
      summaries 
end

let get_summary : prog -> A.t
= fun pgm -> 
	let summary = A.run pgm in
	summary

let match_type
= fun x y ->
  try
      let pass = true in
      let _ = Type.unify Type.Subst.empty (x,y) in pass
  with Type.TypeError -> false

let match_pat 
= fun x y -> true

let match_summary 
= fun (_,t,s) (_,t',s') ->
  let typ_match = match_type t t' in
  let pat_match = match_pat s s' in
  typ_match && pat_match

let rec remove_elem 
= fun e l -> 
  match l with 
  |[] -> []
  |x::xs -> if (match_summary e x) then xs else x :: remove_elem e xs

let rec match_program : A.t -> A.t -> bool
= fun s1 s2 -> 
  match s1,s2 with 
  | [],[] -> true 
  | [],_  
  |  _,[] -> false
  | x::xs, y::ys -> 
    let y'= (remove_elem x (y::ys)) in
    match_program xs y'
    
  (*use is_same_type in main.ml*)
  (*list equality check*)
  (*string * type * summary list*)

(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> prog list -> prog
= fun pgm cpgms -> 
	let cpgm = List.hd cpgms in
	cpgm
