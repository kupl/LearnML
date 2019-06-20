open Lang
open Util

exception NotImplemented

module A = struct
	
	(* summary of function body *)
	type summary = 
		| F of pat list	(* Flat match *)
		| N of (pat * summary) list (* Nested match *)

	(* Function = (function name, type, summary) *)
	type t = (string * typ * summary) list

	(* To string *)
	let rec string_of_summary : summary -> string
	= fun summary ->
		"match {" ^
		match summary with 
		| F ps -> List.fold_left (fun acc p -> acc ^ Print.pat_to_string p ^ " | ") "" ps
		| N summaries -> List.fold_left (fun acc (p, s) -> acc ^ Print.pat_to_string p ^ ":" ^ string_of_summary s ^ " | ") "" summaries
		^ " }"

	let string_of_t : t -> string
	= fun t ->
		List.fold_left (fun acc (name, typ, summary) -> 
			acc ^ "(" ^
			"Name : " ^ name ^ "\n" ^
			"Type : " ^ Print.type_to_string typ ^ "\n" ^
			"Structure : \n" ^ string_of_summary summary ^ "\n)"
		) "" t

    let lookup_type : id -> Type.TEnv.t -> typ
    = fun func_id tenv -> Type.TEnv.find tenv func_id

    let extract_summary : binding -> (string * summary)
    = fun (f,_,_,_,exp) -> 
      let rec get_pattern: lexp -> summary 
      = fun (_,exp) -> 
        match exp with
        | EApp (e1,e2) -> raise NotImplemented
        | EFun (_,e) -> raise NotImplemented 
        | ELet _ -> raise NotImplemented 
        | EBlock _ -> raise NotImplemented
        | EMatch (_,br) -> let pat = List.map (fun (x,y) -> x) br in F (pat)
        | _ -> raise NotImplemented 
      in 
      let name = Print.let_to_string f in
      let summary = get_pattern exp in
      (name, summary)

    let explore_decl : binding list -> decl -> binding list 
    = fun acc decl->
      match decl with
      | DLet binding -> binding :: acc
      | DBlock (_, bindlst) -> bindlst @ acc
      | _ -> acc

    let explore_prog : prog -> binding list
    = fun decls -> List.fold_left explore_decl [] decls

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

let match_summary : A.t -> A.t -> bool
= fun s1 s2 -> true

(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> prog list -> prog
= fun pgm cpgms -> 
	let cpgm = List.hd cpgms in
	cpgm
