open Lang
open Util
open Type

exception NotImplemented

module T = struct
  (* Type annotation *)
  let rec subst_exp : Type.Subst.t -> lexp -> lexp
  = fun subst (l, exp) ->
    let exp = 
      match exp with
      | Raise e -> Raise (subst_exp subst e)
      | EFun (arg, e) -> EFun (arg, subst_exp subst e)
      | MINUS e -> MINUS (subst_exp subst e)
      | NOT e -> NOT (subst_exp subst e)
      | ADD (e1, e2) -> ADD (subst_exp subst e1, subst_exp subst e2)
      | SUB (e1, e2) -> SUB (subst_exp subst e1, subst_exp subst e2)
      | MUL (e1, e2) -> MUL (subst_exp subst e1, subst_exp subst e2)
      | DIV (e1, e2) -> DIV (subst_exp subst e1, subst_exp subst e2)
      | MOD (e1, e2) -> MOD (subst_exp subst e1, subst_exp subst e2)
      | OR (e1, e2) -> OR (subst_exp subst e1, subst_exp subst e2)
      | AND (e1, e2) -> AND (subst_exp subst e1, subst_exp subst e2)
      | LESS (e1, e2) -> LESS (subst_exp subst e1, subst_exp subst e2)
      | LESSEQ (e1, e2) -> LESSEQ (subst_exp subst e1, subst_exp subst e2)
      | LARGER (e1, e2) -> LARGER (subst_exp subst e1, subst_exp subst e2)
      | LARGEREQ (e1, e2) -> LARGEREQ (subst_exp subst e1, subst_exp subst e2)
      | EQUAL (e1, e2) -> EQUAL (subst_exp subst e1, subst_exp subst e2)
      | NOTEQ (e1, e2) -> NOTEQ (subst_exp subst e1, subst_exp subst e2)
      | DOUBLECOLON (e1, e2) -> DOUBLECOLON (subst_exp subst e1, subst_exp subst e2)
      | AT (e1, e2) -> AT (subst_exp subst e1, subst_exp subst e2)
      | STRCON (e1, e2) -> STRCON (subst_exp subst e1, subst_exp subst e2)
      | EApp (e1, e2) -> EApp (subst_exp subst e1, subst_exp subst e2)
      | EList es -> EList (List.map (fun e -> subst_exp subst e) es)
      | ETuple es -> ETuple (List.map (fun e -> subst_exp subst e) es)
      | ECtor (x, es) -> ECtor (x, List.map (fun e -> subst_exp subst e) es)
      | IF (e1, e2, e3) -> IF (subst_exp subst e1, subst_exp subst e2, subst_exp subst e3)
      | EMatch (e, bs) -> EMatch (subst_exp subst e, List.map (fun (p, e) -> (p, subst_exp subst e)) bs)
      | ELet (f, is_rec, args, typ, e1, e2) -> ELet (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e1, subst_exp subst e2)
      | EBlock (is_rec, bindings, e2) -> EBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, Subst.apply typ subst, subst_exp subst e)) bindings, subst_exp subst e2)
      | _ -> exp
    in
    (l, exp)

  let rec subst_decl : Type.Subst.t -> decl -> decl
  = fun subst decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> DLet (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e)
    | DBlock (is_rec, bindings) -> DBlock (is_rec, List.map (fun (f, is_rec, args, typ, e) -> (f, is_rec, args, Type.Subst.apply typ subst, subst_exp subst e)) bindings)
    | _ -> decl

  let run : prog -> prog
  = fun pgm ->
    let (_, _, _, subst) = Type.run pgm in
    List.map (fun decl -> subst_decl subst decl) pgm
end 

module S = struct
	(* Control flow graph *)
  type cfg = 
    | Empty
    | Seq of (cfg * cfg)
    | If of (cfg * cfg * cfg)
    | Match of (pat * cfg) list 

  (* Analysis result = (function name, cfg) mapping *)
  type t = (id * cfg) list

  let empty_env = []
  let extend_env x t env = (x, t)::env

  (* To string *)
  let rec string_of_cfg : cfg -> string
  = fun cfg ->
    match cfg with
    | Empty -> "Empty"
    | Seq (g1, g2) -> "(" ^ string_of_cfg g1 ^ " => " ^ string_of_cfg g2 ^ ")"
    | If (g1, g2, g3) -> 
      "if (" ^ string_of_cfg g1 ^ ")" ^ "(" ^ string_of_cfg g2 ^ ")" ^ "(" ^ string_of_cfg g3 ^ ")"
    | Match bs -> 
      "match (" ^ List.fold_left (fun acc (p, g) -> acc ^ "\n|" ^ Print.pat_to_string p ^ " -> " ^ string_of_cfg g) "" bs ^ ")"
    
  let string_of_t : t -> string
  = fun t ->
    List.fold_left (fun acc (name, cfg) -> 
      acc ^ "(\n" ^
      "Name : " ^ name ^ "\n" ^            
      "CFG : \n" ^ string_of_cfg cfg ^ "\n)"
    ) "" t

  (* CFG extraction *)
  let rec is_fun : typ -> bool
  = fun typ ->
    match typ with
    | TList t -> is_fun t
    | TTuple ts -> List.exists is_fun ts
    | TCtor (t, ts) -> List.exists is_fun (t::ts)
    | TArr _ -> true
    | _ -> false
  
  let rec extract_exp : t -> lexp -> (cfg * t)
  = fun t (l, exp) ->
    match exp with 
    (* Unary *)
    | Raise e | EFun (_, e) | MINUS e | NOT e -> (extract_exp t e)
    (* Binary operation *)
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
    | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2)       
    | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) 
    | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) ->
      let (g1, t) = extract_exp t e1 in
      let (g2, t) = extract_exp t e2 in
      begin match (g1, g2) with
      | g1, Empty -> (g1, t)
      | Empty, g2 -> (g2, t)
      | _ -> (Seq (g1, g2), t)
      end
    (* List *)
    | EList es | ETuple es | ECtor (_, es) -> 
      List.fold_left (fun (g, t) e -> 
        let (g', t) = extract_exp t e in
        match (g, g') with
        | g1, Empty -> (g1, t)
        | Empty, g2 -> (g2, t)
        | _ -> (Seq (g, g'), t) 
      ) (Empty, t) es
    (* Condition *)
    | IF (e1, e2, e3) -> 
      let (g1, t) = extract_exp t e1 in
      let (g2, t) = extract_exp t e2 in
      let (g3, t) = extract_exp t e3 in
      (If (g1, g2, g3), t)
    | EMatch (e, bs) -> 
      let rec flatten_branch : branch list -> branch list
      = fun bs ->
        match bs with
        | [] -> []
        | (p, e)::bs -> 
          begin match p with
          | Pats ps -> 
            let flat_bs = (List.map (fun p -> (p, e)) ps) in
            (flatten_branch flat_bs)@(flatten_branch bs)
          | _ -> (p, e)::(flatten_branch bs)
          end
      in
      let bs = flatten_branch bs in
      let (g, t) = extract_exp t e in
      let (bs, t) = 
        List.fold_left (fun (bs, t) (p, e) -> 
          let (g, t) = extract_exp t e in
          ((p, g)::bs, t)
        ) ([], t) bs
      in
      (Seq (g, Match (List.rev bs)), t)
    (* Binding *)
    | ELet (f, is_rec, args, typ, e1, e2) -> 
      if args <> [] || is_fun typ then
        begin match f with
        | BindOne f ->
          let (g1, t) = extract_exp t e1 in
          extract_exp (extend_env f g1 t) e2
        | _ -> raise (Failure "Invalid function format")
        end
      else extract_exp t e2
    | EBlock (is_rec, bindings, e2) -> 
      let t = List.fold_left (fun t (f, is_rec, args, typ, e) -> 
        if args <> [] || is_fun typ then
          begin match f with
          | BindOne f ->
            let (g1, t) = extract_exp t e in
            extend_env f g1 t
          | _ -> raise (Failure "Invalid function format")
          end
        else t
      ) t bindings in
      extract_exp t e2
    (* Const *)
    | _ -> (Empty, t)

  let rec extract_decl : t -> decl -> t
  = fun t decl ->
    match decl with
    | DLet (f, is_rec, args, typ, e) -> 
      if args <> [] || is_fun typ then
        begin match f with
        | BindOne f ->
          let (g, t) = extract_exp t e in
          (extend_env f g t)
        | _ -> raise (Failure "Invalid function format")
        end
      else t
    | DBlock (is_rec, bindings) -> List.fold_left (fun t binding -> extract_decl t (DLet binding)) t bindings
    | _ -> t

  (* Extract cfg of a given program *)
	let run : prog -> t
	= fun pgm -> 
    T.run pgm 
    |> List.fold_left (fun t decl -> (extract_decl t decl)) empty_env 
end

let rec match_pair_list : ('b -> 'b -> bool) -> ('a * 'b) list -> ('a * 'b) list -> bool
= fun compare lst1 lst2 ->
  match lst1, lst2 with
  | [], [] -> true
  | (a1, b1)::tl1, (a2, b2)::tl2 ->
    begin
      try
        let (a2, b2) = List.find (fun (a2, b2) -> compare b1 b2) lst2 in
        match_pair_list compare tl1 (List.remove_assoc a2 lst2)
      with Not_found -> false
    end
  | _ -> false

let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  (* TODO *)
  (* print_endline ("P1 : " ^ Print.pat_to_string p1 ^ ", " ^ "P2 : " ^ Print.pat_to_string p2); *)
  match (p1, p2) with
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 | PCons ps1, PCons ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | Pats ps1, Pats ps2 -> raise (Failure "Invalid pattern mathcing of two cfgs")
  | _ -> false

let rec match_cfg : S.cfg -> S.cfg -> bool
= fun g1 g2 ->
  match (g1, g2) with
  | S.Empty, S.Empty -> true
  | S.Seq (g1, g2), S.Seq (g1', g2') -> ((match_cfg g1 g1') && (match_cfg g2 g2')) || ((match_cfg g1 g2') && (match_cfg g2 g1'))
  | S.If (g1, g2, g3), S.If (g1', g2', g3') -> 
    if (match_cfg g1 g1') then ((match_cfg g2 g2') && (match_cfg g3 g3')) || ((match_cfg g2 g3') && (match_cfg g3 g2')) else false
  | S.Match bs1, S.Match bs2 ->
    (* Check *)
    (try List.for_all2 (fun (p1, g1) (p2, g2) -> (match_pat p1 p2) && (match_cfg g1 g2)) (List.sort compare bs1) (List.sort compare bs2) with _ -> false)
  | _ -> false

let rec match_t : S.t -> S.t -> bool
= fun t1 t2 -> match_pair_list match_cfg t1 t2

let log = ref (open_out "cfg.txt")
(*
	Input : An incorrect program pgm and a set of correct programs cpgms
	Output : A correct program cpgm which is most similar to pgm
*)
let run : prog -> (string*prog) list -> (string * prog option) list
= fun pgm cpgms -> 
  let t = S.run pgm in
  let results = List.map (fun (s,cpgm)-> (s,cpgm, S.run cpgm)) cpgms in
  
  (*
  List.iter (fun (s,cpgm, t') ->
    Printf.fprintf (!log) "------------Program-------------\n%s\n" (Print.program_to_string cpgm);
    Printf.fprintf (!log) "----------Analysis_Result-------\n%s\n" (S.string_of_t t')
  ) results;
  *)
  try 
  	let matched = List.filter (fun (s,cpgm, t') -> 
      (*
      Print.print_header "Analysis1"; print_endline (S.string_of_t t);
      Print.print_header "Analysis2"; print_endline (S.string_of_t t');
      *)
      match_t t t'
    ) results in
    List.map (fun (s,cpgm,t') -> (s,Some cpgm)) matched     
  with Not_found -> [("",None);]
