open Lang
open Util
open Type

exception NotImplemented

(* Control flow graph *)
type cfg = 
  | Empty
  | Seq of cfg * cfg
  | If of cfg * cfg * cfg
  | Match of cfg * (pat * cfg) list 

type t = (id, cfg) BatMap.t

type matching = (cfg * cfg) list

(* To string *)
let rec string_of_cfg : cfg -> string
= fun cfg ->
  match cfg with
  | Empty -> "X"
  | Seq (g1, g2) -> string_of_cfg g1 ^ ";\n" ^ string_of_cfg g2
  | If (g1, g2, g3) -> 
    "if (" ^ string_of_cfg g1 ^ ")" ^ " (" ^ string_of_cfg g2 ^ ")" ^ " (" ^ string_of_cfg g3 ^ ")"
  | Match (g, bs) -> 
    "match" ^ " (" ^ string_of_cfg g ^ ") with" ^ 
    List.fold_left (fun acc (p, g) -> acc ^ "\n|" ^ Print.pat_to_string p ^ " -> " ^ string_of_cfg g) "" bs ^ ")"
  
let string_of_t : t -> string
= fun t ->
  BatMap.foldi (fun id cfg acc -> 
    acc ^ "----------------\n" ^
    "Name : " ^ id ^ "\n" ^            
    "CFG : \n" ^ string_of_cfg cfg ^ "\n"
  ) t ""

let print t = print_endline (string_of_t t)

let print_matching matching =
  List.iter (fun (cfg1, cfg2) ->
    print_endline ("CFG1 : \n");
    print_endline (string_of_cfg cfg1 ^ "\n"); 
    print_endline ("CFG2 : \n");
    print_endline (string_of_cfg cfg2 ^ "\n")
  ) matching
(* CFG extraction *)
let rec is_fun : typ -> bool
= fun typ ->
  match typ with
  | TArr _ -> true
  | _ -> false

let join_cfg : cfg -> cfg -> cfg
= fun g1 g2 ->
  match (g1, g2) with
  | g, Empty | Empty, g -> g
  | _ -> Seq (g1, g2)
  
let rec exp_to_cfg : lexp -> cfg
= fun (l, exp) ->
  match exp with 
  (* Unary *)
  | Raise e | EFun (_, e) | MINUS e | NOT e -> exp_to_cfg e
  (* Binary operation *)
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) 
  | OR (e1, e2) | AND (e1, e2) | LESS (e1, e2) | LESSEQ (e1, e2) | LARGER (e1, e2)       
  | LARGEREQ (e1, e2) | EQUAL (e1, e2) | NOTEQ (e1, e2) 
  | DOUBLECOLON (e1, e2) | AT (e1, e2) | STRCON (e1, e2) | EApp (e1, e2) ->
    let (g1, g2) = (exp_to_cfg e1, exp_to_cfg e2) in
    join_cfg g1 g2
  (* List *)
  | EList es | ETuple es | ECtor (_, es) -> 
    List.fold_left (fun g e -> 
      let g' = exp_to_cfg e in
      join_cfg g g'
    ) Empty es
  (* Condition *)
  | IF (e1, e2, e3) -> 
    let (g1, g2, g3) = (exp_to_cfg e1, exp_to_cfg e2, exp_to_cfg e3) in
    If (g1, g2, g3)
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
    let g = exp_to_cfg e in
    let bs = List.map (fun (p, e) -> (p, exp_to_cfg e)) (flatten_branch bs) in
    Match (g, bs)
  (* Binding *)
  | ELet (f, is_rec, args, typ, e1, e2) -> 
    if args <> [] || is_fun typ then raise (Failure "Normalized program should not include functions")
    else 
      let (g1, g2) = (exp_to_cfg e1, exp_to_cfg e2) in
      join_cfg g1 g2
  | EBlock (is_rec, bindings, e2) -> 
    if List.exists (fun (f, is_rec, args, typ, e) -> args <> [] || is_fun typ) bindings then
      raise (Failure "Normalized program should not include functions")
    else 
      let g1 = List.fold_left (fun g (f, is_rec, args, typ, e) -> 
        let g' = exp_to_cfg e in
        join_cfg g g'
      ) Empty bindings in
      let g2 = exp_to_cfg e2 in
      join_cfg g1 g2 
  (* Const *)
  | _ -> Empty

(* Extract cfg of a given program *)
let extract_cfg : prog -> t
= fun pgm -> 
  let npgm = Normalizer.normalize_all pgm in
  BatMap.map (fun (args, typ, body) -> exp_to_cfg body) npgm

(******************************************)
(************** CFG matching **************)
(******************************************)
let rec match_pat : pat -> pat -> bool
= fun p1 p2 ->
  match (p1, p2) with
  | PUnit, PUnit | PUnder, PUnder | PVar _, PVar _ -> true
  | PInt n1, PInt n2 -> n1 = n2
  | PBool b1, PBool b2 -> b1 = b2
  | PList ps1, PList ps2 | PTuple ps1, PTuple ps2 -> (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCtor (x, ps1), PCtor (y, ps2) -> (x = y) && (try List.for_all2 match_pat ps1 ps2 with _ -> false)
  | PCons (phd1, ptl1), PCons (phd2, ptl2) -> match_pat phd1 phd2 && match_pat ptl1 ptl2
  | Pats ps1, Pats ps2 -> raise (Failure "Invalid pattern mathcing of two cfgs")
  | _ -> false

let rec match_cfg : cfg -> cfg -> bool
= fun g1 g2 ->
  match (g1, g2) with
  | Empty, Empty -> true
  | Seq (g1, g2), Seq (g1', g2') -> (match_cfg g1 g1') && (match_cfg g2 g2')
  | If (g1, g2, g3), If (g1', g2', g3') -> 
    if (match_cfg g1 g1') then ((match_cfg g2 g2') && (match_cfg g3 g3')) || ((match_cfg g2 g3') && (match_cfg g3 g2')) else false
  | Match (g1, bs1), Match (g2, bs2) ->
    if (match_cfg g1 g2) && (List.length bs1 = List.length bs2) then
      begin match (list_match (fun (p1, cfg1) (p2, cfg2) -> match_pat p1 p2 && match_cfg cfg1 cfg2) bs1 bs2) with
      | None -> false
      | _ -> true
      end
    else false 
  | _ -> false

let rec match_t : t -> t -> matching option
= fun t1 t2 ->
  let extract_body t = List.map (fun (id, body) -> body) (BatMap.bindings t) in
  let (t1, t2) = (extract_body t1, extract_body t2) in
  list_match match_cfg t1 t2

(* Given two programs finds a matching relation *)
let rec exact_matching : prog -> prog -> matching option
= fun pgm1 pgm2 ->
  let (t1, t2) = (extract_cfg pgm1, extract_cfg pgm2) in
  match_t t1 t2