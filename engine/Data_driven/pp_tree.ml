open Lang
open Util
open Symbol_lang

(*****************************)
(*********tree print**********)
(*****************************)
let depth d'= 
  let rec aux d = 
  match d with
  | 0 -> ""
  | _ -> "-"^(aux (d-1))
  in "\n"^aux d'

let rec pat_to_tree : int -> pat -> string
  = fun d pat ->
    match pat with
    | PUnit -> "PUnit ()"
    | PCtor (x,lst) -> "PCtor " ^ x ^ (if lst=[] then "" else " " ^ pat_to_tree d (List.hd lst))
    | Pats lst ->
      "Pats " ^ list_fold (fun p r -> r ^ "|"^(Print.pat_to_string p)^" ") lst ""
    | PInt n -> "PInt "^string_of_int n
    | PVar x -> "PVar "^ x
    | PBool b -> "PBool " ^ if (b) then "true" else "false"
    | PList lst -> "PList "^ Print.pp_list Print.pat_to_string lst
    | PTuple lst -> "PTuple "^Print.pp_tuple Print.pat_to_string lst
    | PUnder -> "PUnder _"
    | PCons (lst) ->
      begin match lst with
        |[] -> raise (Failure "Pattern Cons does not have arguments")
        |hd::tl ->
          "PCons "^ (pat_to_tree d hd)^(list_fold (fun p r -> r^"::"^(Print.pat_to_string p)) tl "")
      end

let rec type_to_tree : int -> typ -> string =
  fun d ty' ->
  let rec aux ty =
    match ty with
    | TUnit -> "unit"
    | TInt -> "int"
    | TString -> "string"
    | TBool -> "bool"
    | TBase id -> id
    | TList t -> aux t ^ " list"
    | TTuple l -> if(l=[]) then "unit" else Print.pp_star_tuple aux l
    | TCtor (t, l) -> aux t ^ (if(l=[]) then "" else Print.pp_tuple aux l)
    | TArr (t1,t2) -> "(" ^ aux t1 ^ " -> " ^ aux t2 ^ ")"
    | TVar x -> x
    | TExn -> "exn"
  in "Type "^ aux ty' 
      
let rec let_to_tree : let_bind -> string
= fun x ->
  match x with 
  | BindUnder -> "BindUnder"
  | BindOne x -> "BindOne " ^ "\"" ^ x ^ "\""
  | BindTuple xs -> "BindTuple " ^ Print.pp_tuple let_to_tree xs

let rec arg_to_tree : int -> arg -> string
= fun d arg ->
  match arg with
  | ArgUnder typ ->
    (match typ with 
    | TVar _ -> "ArgUnder TVar _" 
    | _ -> "ArgUnder \"" ^ "_" ^ " : " ^ type_to_tree d typ ^ "\"" )
  | ArgOne (x,typ) -> 
    (match typ with
    | TVar _ -> "ArgOne Tvar \""^ x ^ "\""
    | _ -> "ArgOne \"" ^ x ^ " : " ^ type_to_tree d typ ^ "\"" )
  | ArgTuple xs -> Print.pp_tuple (arg_to_tree d) xs

let rec args_to_tree : int -> arg list -> string -> string
= fun d args str ->
  list_fold (fun arg r -> r ^ arg_to_tree d arg ^ " ") args str

let rec exp_to_tree : int -> lexp -> string
= fun d (_,exp) ->
  match exp with
  |SInt n -> "SInt"
  |SStr n -> "SStr" (*^ "(" ^string_of_int n^")"*)
  |EUnit -> "EUnit"
  |Const n -> "Const "^string_of_int n
  |String id -> "\"" ^ id ^"\""
  |TRUE -> "True"
  |FALSE -> "False"
  |EVar x ->  "\""^ x ^"\""
  |EList lst -> Print.pp_list (exp_to_tree d) lst
  |ETuple lst -> Print.pp_tuple (exp_to_tree d) lst
  |ECtor (x,lst) -> "ECtor " ^ x ^ (if lst=[] then "" else " (" ^ exp_to_tree d (List.hd lst) ^ ")") 
  |ADD (e1,e2) -> "ADD (" ^ exp_to_tree d e1 ^ " + " ^ exp_to_tree d e2 ^")"  
  |SUB (e1,e2) -> "SUB (" ^ exp_to_tree d e1 ^ " - " ^ exp_to_tree d e2 ^")"  
  |MUL (e1,e2) -> "MUL (" ^ exp_to_tree d e1 ^ " * " ^ exp_to_tree d e2 ^")"  
  |DIV (e1,e2) -> "DIV (" ^ exp_to_tree d e1 ^ " / " ^ exp_to_tree d e2 ^")"  
  |MOD (e1,e2) -> "MOD (" ^ exp_to_tree d e1 ^ " mod " ^ exp_to_tree d e2 ^")"  
  |MINUS (e) -> "MINUS -(" ^ exp_to_tree d e ^ ")"
  |OR (e1,e2) -> "OR (" ^ exp_to_tree d e1 ^ " || " ^ exp_to_tree d e2 ^")"
  |AND (e1,e2) -> "AND (" ^ exp_to_tree d e1 ^ " && " ^ exp_to_tree d e2 ^")"
  |LESS (e1,e2) -> "LESS (" ^ exp_to_tree d e1 ^ " < " ^ exp_to_tree d e2 ^")"
  |LARGER (e1,e2) -> "LT (" ^  exp_to_tree d e1 ^ " > " ^ exp_to_tree d e2 ^")"
  |LARGEREQ (e1,e2) -> "LE (" ^ exp_to_tree d e1 ^ " >= " ^ exp_to_tree d e2 ^")"
  |EQUAL (e1,e2) -> "EQ (" ^ exp_to_tree d e1 ^ " = " ^ exp_to_tree d e2 ^")"
  |NOTEQ (e1,e2) -> "NEQ (" ^ exp_to_tree d e1 ^ " != " ^ exp_to_tree d e2 ^")"
  |LESSEQ (e1,e2) -> "LEQ (" ^ exp_to_tree d e1 ^ " <= " ^ exp_to_tree d e2 ^")"
  |AT (e1,e2) -> "AT (" ^ exp_to_tree d e1 ^ " @ " ^ exp_to_tree d e2 ^")"
  |DOUBLECOLON (e1,e2) -> "(" ^ exp_to_tree d e1 ^ " :: " ^ exp_to_tree d e2 ^")"
  |STRCON (e1,e2) -> "CONCAT (" ^ exp_to_tree d e1 ^ " ^ " ^ exp_to_tree d e2 ^ ")"
  |NOT e -> "not (" ^ exp_to_tree d e ^ ")"
  |EApp (e1,e2) -> 
    "EApp (" ^ exp_to_tree (d+2) e1 ^ " " ^  "(" ^ exp_to_tree (d+2) e2 ^ ")" ^ ")"
  |IF (e1,e2,e3) -> 
    depth d^"IF "^exp_to_tree (d+2) e1 ^ 
    depth d^"THEN "^exp_to_tree (d+2) e2 ^
    depth d^"ELSE "^exp_to_tree (d+2) e3
  |ELet (f, is_rec, xs, t, e1, e2) -> 
    "ELet " ^ (if is_rec then "rec " else "") ^
    binding_to_tree (d+2) (f, is_rec, xs, t, e1) ^ " in" ^ (exp_to_tree (d+2) e2)
  |EBlock (is_rec, es, e2) ->
    "EBlock let " ^
    (if is_rec then "rec " else "") ^
    Print.pp_block (binding_to_tree (d+2)) es ^ "" ^ " in"^ exp_to_tree (d+2) e2 
  |EFun (arg,e1) -> 
    let rec multi_args (e : lexp) r =
      begin match snd e with
      |EFun (a,exp) -> 
        let arg_seq = r ^ " " ^ arg_to_tree d a in
        multi_args exp arg_seq
      |_ -> "EFun " ^ r ^ " -> " ^ exp_to_tree d e
    end in
    multi_args e1 (arg_to_tree d arg)
  |EMatch (e,lst) ->  
    "EMatch " ^ exp_to_tree (d+2) e ^ 
    (list_fold (fun (p,e) r -> r ^ depth d ^ "|" ^ pat_to_tree (d+2) p ^ " -> " ^ exp_to_tree (d+2) e) lst "")
  |Hole n -> "?"
  |Raise e -> "raise "^Print.exp_to_string e

and binding_to_tree : int -> binding -> string
= fun d (f,is_rec,args,typ,exp) ->
  depth d ^ (let_to_tree f) ^ " " ^ depth d ^ args_to_tree d args "" ^ 
  (match typ with | TVar _ -> "" 
                  | _ -> "" ^ type_to_tree d typ)
  ^ " = " ^ depth d ^ (exp_to_tree (d+2) exp)

let decl_to_tree: int -> decl -> string -> string
= fun d decl str-> 
  match decl with 
  | DExcept ctor -> 
    str ^ "\n" ^ "DExcept" ^ Print.user_defined_type_to_string ctor ^ "\n"
  | DEqn (x, typ) ->
    str ^ "DEqn type " ^ x ^ " = " ^ Print.type_to_string typ ^ "\n"
  | DData (id,lst) ->
    str ^ "DData type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ Print.user_defined_type_to_string t) lst "") ^ "\n"
  | DLet (f, is_rec, args, typ, exp) -> 
    str ^ "\n" ^ "DLet " ^ (if is_rec then "Rec" else "") ^
    binding_to_tree (d+2) (f, is_rec, args, typ, exp)
  | DBlock (is_rec, ds) ->
    str ^ "\n" ^ "" ^ "DBlock Let " ^ (if is_rec then "Rec " else "") ^
    Print.pp_block (binding_to_tree (d+2)) ds ^ ""
  | TBlock decls -> str

let program_to_tree : int -> prog -> string
= fun d prog -> list_fold (decl_to_tree d) prog ""
