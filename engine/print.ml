open Lang
open Util
open Symbol_lang

(******** Format **********)
let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="[") ?(last="]") ?(sep=";") : ('a -> string) -> ('a list) -> string
= fun string_of_v lst -> 
  let add_string_of_v acc v = link_by_sep sep (string_of_v v) acc in
  first ^ List.fold_left add_string_of_v "" lst ^ last

let string_of_set ?(first="{") ?(last="}") ?(sep=",") : ('a -> string) -> ('a BatSet.t) -> string
= fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last

let string_of_map ?(first="{") ?(last="}") ?(sep=",\n") : ('a -> string) -> ('b -> string) -> (('a, 'b) BatMap.t) -> string
= fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep sep str acc 
  in
  if BatMap.is_empty map then "empty"
  else first ^ BatMap.foldi add_string_of_k_v map "" ^ last

let pp_tuple : ('a -> string) -> 'a list -> string
= fun func tup ->
  match tup with
  |[] -> "()"
  |hd::tl -> 
    "(" ^ (func hd) ^
    (list_fold (fun elem r -> r ^ ", " ^ (func elem)) tl "") ^ ")"

let pp_list : ('a -> string) -> 'a list -> string
= fun func lst ->
  match lst with
  |[] -> "[]"
  |hd::tl ->
    "[" ^ (func hd) ^
    (list_fold (fun elem r -> r ^ "; " ^ (func elem)) tl "") ^ "]"

let pp_star_tuple : ('a -> string) -> 'a list -> string
= fun func tuple ->
  match tuple with
  | [] -> ""
  | hd::tl ->
    "(" ^ (func hd) ^
    (list_fold (fun elem r -> r ^ " * " ^ (func elem)) tl "") ^ ")"

let pp_block : ('a -> string) -> 'a list -> string
= fun func decls ->
  match decls with
  | [] -> ""
  | hd::tl ->
    (func hd) ^
    List.fold_left (fun str elem -> str ^ "and " ^ (func elem)) "" tl

let rec pat_to_string : pat -> string
= fun pat ->
  match pat with
  | PUnit -> "()"
  | PCtor (x,lst) -> x ^ (if lst=[] then "" else " " ^ pat_to_string (List.hd lst))
  | Pats lst ->
    list_fold (fun p r -> r ^ "|"^(pat_to_string p)^" ") lst ""
  | PInt n -> string_of_int n
  | PVar x -> x
  | PBool b -> if (b) then "true" else "false"
  | PList lst -> pp_list pat_to_string lst
  | PTuple lst -> pp_tuple pat_to_string lst
  | PUnder -> "_"
  | PCons (p1, p2) -> pat_to_string p1 ^ "::" ^ pat_to_string p2

let rec type_to_string : typ -> string
= fun ty -> 
  match ty with
  | TUnit -> "unit"
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"
  | TBase id -> id
  | TList t -> type_to_string t ^ " list"
  | TTuple l -> if(l=[]) then "unit" else pp_star_tuple type_to_string l
  | TCtor (t, l) -> type_to_string t ^ (if(l=[]) then "" else pp_tuple type_to_string l)
  | TArr (t1,t2) -> "(" ^ type_to_string t1 ^ " -> " ^ type_to_string t2 ^ ")"
  | TVar x -> x
  | TExn -> "exn"
  | TRef typ -> type_to_string typ ^ " " ^ "ref"

let rec arg_to_string : arg -> string
= fun arg ->
  match arg with
  | ArgUnder typ -> 
    begin match typ with
    | TVar _ -> "_"
    | _ -> "(" ^ "_" ^ " : " ^ type_to_string typ ^ ")"
    end
  | ArgOne (x, typ) -> 
    begin match typ with
    | TVar _ -> x
    | _ -> "(" ^ x ^ " : " ^ type_to_string typ ^ ")"
    end
  | ArgTuple xs -> pp_tuple arg_to_string xs

let rec args_to_string : arg list -> string -> string
= fun args str ->
  list_fold (fun arg r -> r ^ arg_to_string arg ^ " ") args str

let rec user_defined_type_to_string : ctor -> string
= fun (id,typ_lst) -> 
  match typ_lst with
  |[] -> id
  |hd::tl -> id ^ " of " ^ type_to_string hd ^ 
  list_fold (fun t r-> r ^ " * " ^ type_to_string t) tl ""

let rec let_to_string : let_bind -> string
= fun x ->
  match x with
  | BindUnder -> "_"
  | BindOne x -> x
  | BindTuple xs -> pp_tuple let_to_string xs

let library_functions = [
  ("__list_hd__", "List.hd");
  ("__list_tl__", "List.tl");
  ("__list_map__", "List.map");
  ("__list_mem__", "List.mem");
  ("__list_exists__", "List.exists");
  ("__list_filter__", "List.filter");
  ("__list_append__", "List.append");
  ("__list_length__", "List.length");
  ("__list_nth__", "List.nth");
  ("__list_rev__", "List.rev");
  ("__list_fold_left__", "List.fold_left");
  ("__list_fold_right__", "List.fold_right");
  ("__list_sort__", "List.sort");
  ("__list_rev_map__", "List.rev_map");
  ("__list_memq__", "List.memq");
  ("__list_rev_append__", "List.rev_append");
  ("__list_mapi__", "List.mapi");
  ("__list_for_all__", "List.for_all");
  ("__list_find__", "List.find");
  ("__list_assoc__", "List.assoc");
  ("__string_concat__", "String.concat");
]

let rec exp_to_string : lexp -> string
= fun (l, exp) ->
  match exp with
  |SInt n -> "#A"
  |SStr n -> "#S" (*^ "(" ^string_of_int n^")"*)
  |EUnit -> "()"
  |Const n -> string_of_int n
  |String id -> "\"" ^ id ^"\""
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> (try List.assoc x library_functions with _ -> x)
  |EList lst -> pp_list exp_to_string lst
  |ETuple lst -> pp_tuple exp_to_string lst
  |ECtor (x,lst) -> x ^ (if lst=[] then "" else " (" ^ exp_to_string (List.hd lst) ^ ")") 
  |ADD (e1,e2) -> "(" ^ exp_to_string e1 ^ " + " ^ exp_to_string e2 ^")"  
  |SUB (e1,e2) -> "(" ^ exp_to_string e1 ^ " - " ^ exp_to_string e2 ^")"  
  |MUL (e1,e2) -> "(" ^ exp_to_string e1 ^ " * " ^ exp_to_string e2 ^")"  
  |DIV (e1,e2) -> "(" ^ exp_to_string e1 ^ " / " ^ exp_to_string e2 ^")"  
  |MOD (e1,e2) -> "(" ^ exp_to_string e1 ^ " mod " ^ exp_to_string e2 ^")"  
  |MINUS (e) -> "-(" ^ exp_to_string e ^ ")"
  |OR (e1,e2) -> "(" ^ exp_to_string e1 ^ " || " ^ exp_to_string e2 ^")"
  |AND (e1,e2) -> "(" ^ exp_to_string e1 ^ " && " ^ exp_to_string e2 ^")"
  |LESS (e1,e2) -> "(" ^ exp_to_string e1 ^ " < " ^ exp_to_string e2 ^")"
  |LARGER (e1,e2) -> "(" ^ exp_to_string e1 ^ " > " ^ exp_to_string e2 ^")"
  |LARGEREQ (e1,e2) -> "(" ^ exp_to_string e1 ^ " >= " ^ exp_to_string e2 ^")"
  |EQUAL (e1,e2) -> "(" ^ exp_to_string e1 ^ " = " ^ exp_to_string e2 ^")"
  |NOTEQ (e1,e2) -> "(" ^ exp_to_string e1 ^ " != " ^ exp_to_string e2 ^")"
  |LESSEQ (e1,e2) -> "(" ^ exp_to_string e1 ^ " <= " ^ exp_to_string e2 ^")"
  |AT (e1,e2) -> "(" ^ exp_to_string e1 ^ " @ " ^ exp_to_string e2 ^")"
  |DOUBLECOLON (e1,e2) -> "(" ^ exp_to_string e1 ^ " :: " ^ exp_to_string e2 ^")"
  |STRCON (e1,e2) -> "(" ^ exp_to_string e1 ^ " ^ " ^ exp_to_string e2 ^ ")"
  |NOT e -> "not (" ^ exp_to_string e ^ ")"
  |EApp (e1,e2) -> 
    exp_to_string e1 ^ " " ^
    begin match snd e2 with
    |EApp _ -> "(" ^ exp_to_string e2 ^ ")"
    |_ -> "(" ^ exp_to_string e2 ^ ")"
    end
  |IF (e1,e2,e3) -> 
    "if " ^ exp_to_string e1 ^ 
    " then " ^ exp_to_string e2 ^ "\n" ^
    " else " ^ exp_to_string e3
  |ELet (f, is_rec, xs, t, e1, e2) -> 
    "\n" ^ "let " ^ (if is_rec then "rec " else "") ^
    binding_to_string (f, is_rec, xs, t, e1) ^ " in \n" ^ (exp_to_string e2)
  |EBlock (is_rec, es, e2) ->
    "\n" ^ "let " ^
    (if is_rec then "rec " else "") ^
    pp_block (binding_to_string) es ^ "" ^ " in \n" ^ exp_to_string e2 
  |EFun (arg,e1) -> 
    let rec multi_args (e : lexp) r =
      begin match snd e with
      |EFun (a,exp) -> 
        let arg_seq = r ^ " " ^ arg_to_string a in
        multi_args exp arg_seq
      |_ -> "\nfun " ^ r ^ " -> " ^ exp_to_string e
    end in
    multi_args e1 (arg_to_string arg)
  |EMatch (e,lst) ->  
    "\n (match " ^ exp_to_string e ^ " with " ^ 
    (list_fold (fun (p,e) r -> r ^ "\n|" ^ pat_to_string p ^ " -> " ^ 
    exp_to_string e) lst "") ^ ")"
  |Hole n -> if n >= 0 then "?" else "<?>"
  |Raise e -> "raise "^exp_to_string e
  | ERef e -> "ref " ^ exp_to_string e
  | EDref e -> "!" ^ exp_to_string e
  | EAssign (e1, e2) -> "(" ^ exp_to_string e1 ^ " := " ^ exp_to_string e2 ^ ")"


  
and binding_to_string : binding -> string
= fun (f, is_rec, args, typ, exp) ->
    (let_to_string f) ^ " " ^ args_to_string args "" ^
    (match typ with |TVar _ -> "" |_ -> " : " ^ type_to_string typ) ^
    " = " ^ (exp_to_string exp) ^ "\n"

let rec decl_to_string : decl -> string -> string
= fun decl str ->
  match decl with
  | DExcept ctor -> 
    str ^ "\n" ^ "exception " ^ user_defined_type_to_string ctor ^ "\n"
  | DEqn (x, typ) ->
    str ^ "type " ^ x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    str ^ "type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> "\n|" ^ user_defined_type_to_string t ^ r) lst "")
    ^ "\n"
  | DLet (f, is_rec, args, typ, exp) -> 
    str ^ "\n" ^ "let " ^ (if is_rec then "rec " else "") ^
    binding_to_string (f, is_rec, args, typ, exp)
  | DBlock (is_rec, ds) ->
    str ^ "\n" ^ "" ^ "let " ^ (if is_rec then "rec " else "") ^
    pp_block (binding_to_string) ds ^ ""
  | TBlock decls ->
    str ^ "\n" ^ "" ^ "type " ^
    pp_block (type_decl_to_string) decls ^ ""

and type_decl_to_string : decl -> string
= fun decl ->
  match decl with
  | DEqn (x, typ) -> x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    id ^ " =" ^ 
    (list_fold (fun t r -> "\n|" ^ user_defined_type_to_string t ^ r) lst "") ^ "\n"
  | _ -> raise (Failure "Invalid type decl")

let program_to_string : prog -> string
= fun prog -> list_fold decl_to_string prog ""

(* Labeled version printer *)
let rec lexp_to_string : lexp -> string
= fun (l, exp) ->
  "(" ^ (string_of_int l) ^ ", " ^
  match exp with
  |SInt n -> "#A"
  |SStr n -> "#S" (*^ "(" ^string_of_int n^")"*)
  |EUnit -> "()"
  |Const n -> string_of_int n
  |String id -> "\"" ^ id ^"\""
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> x
  |EList lst -> pp_list lexp_to_string lst
  |ETuple lst -> pp_tuple lexp_to_string lst
  |ECtor (x,lst) -> x ^ (if lst=[] then "" else " (" ^ lexp_to_string (List.hd lst) ^ ")") 
  |ADD (e1,e2) -> "(" ^ lexp_to_string e1 ^ " + " ^ lexp_to_string e2 ^")"  
  |SUB (e1,e2) -> "(" ^ lexp_to_string e1 ^ " - " ^ lexp_to_string e2 ^")"  
  |MUL (e1,e2) -> "(" ^ lexp_to_string e1 ^ " * " ^ lexp_to_string e2 ^")"  
  |DIV (e1,e2) -> "(" ^ lexp_to_string e1 ^ " / " ^ lexp_to_string e2 ^")"  
  |MOD (e1,e2) -> "(" ^ lexp_to_string e1 ^ " mod " ^ lexp_to_string e2 ^")"  
  |MINUS (e) -> "-(" ^ lexp_to_string e ^ ")"
  |OR (e1,e2) -> "(" ^ lexp_to_string e1 ^ " || " ^ lexp_to_string e2 ^")"
  |AND (e1,e2) -> "(" ^ lexp_to_string e1 ^ " && " ^ lexp_to_string e2 ^")"
  |LESS (e1,e2) -> "(" ^ lexp_to_string e1 ^ " < " ^ lexp_to_string e2 ^")"
  |LARGER (e1,e2) -> "(" ^ lexp_to_string e1 ^ " > " ^ lexp_to_string e2 ^")"
  |LARGEREQ (e1,e2) -> "(" ^ lexp_to_string e1 ^ " >= " ^ lexp_to_string e2 ^")"
  |EQUAL (e1,e2) -> "(" ^ lexp_to_string e1 ^ " = " ^ lexp_to_string e2 ^")"
  |NOTEQ (e1,e2) -> "(" ^ lexp_to_string e1 ^ " != " ^ lexp_to_string e2 ^")"
  |LESSEQ (e1,e2) -> "(" ^ lexp_to_string e1 ^ " <= " ^ lexp_to_string e2 ^")"
  |AT (e1,e2) -> "(" ^ lexp_to_string e1 ^ " @ " ^ lexp_to_string e2 ^")"
  |DOUBLECOLON (e1,e2) -> "(" ^ lexp_to_string e1 ^ " :: " ^ lexp_to_string e2 ^")"
  |STRCON (e1,e2) -> "(" ^ lexp_to_string e1 ^ " ^ " ^ lexp_to_string e2 ^ ")"
  |NOT e -> "not (" ^ lexp_to_string e ^ ")"
  |EApp (e1,e2) -> 
    lexp_to_string e1 ^ " " ^
    begin match snd e2 with
    |EApp _ -> "(" ^ lexp_to_string e2 ^ ")"
    |_ -> "(" ^ lexp_to_string e2 ^ ")"
    end
  |IF (e1,e2,e3) -> 
    "if " ^ lexp_to_string e1 ^ 
    " then " ^ lexp_to_string e2 ^ "\n" ^
    " else " ^ lexp_to_string e3
  |ELet (f, is_rec, xs, t, e1, e2) -> 
    "\n" ^ "let " ^ (if is_rec then "rec " else "") ^
    binding_to_string2 (f, is_rec, xs, t, e1) ^ " in \n" ^ (lexp_to_string e2)
  |EBlock (is_rec, es, e2) ->
    "\n" ^ "let " ^
    (if is_rec then "rec " else "") ^
    pp_block (binding_to_string2) es ^ "" ^ " in \n" ^ lexp_to_string e2 
  |EFun (arg,e1) -> 
    let rec multi_args (e : lexp) r =
      begin match snd e with
      |EFun (a,exp) -> 
        let arg_seq = r ^ " " ^ arg_to_string a in
        multi_args exp arg_seq
      |_ -> "\nfun " ^ r ^ " -> " ^ lexp_to_string e
    end in
    multi_args e1 (arg_to_string arg)
  |EMatch (e,lst) ->  
    "\n (match " ^ lexp_to_string e ^ " with " ^ 
    (list_fold (fun (p,e) r -> r ^ "\n|" ^ pat_to_string p ^ " -> " ^ 
    lexp_to_string e) lst "") ^ ")"
  |Hole n -> "? (" ^ string_of_int n ^ ")"
  |Raise e -> "raise "^lexp_to_string e
  | ERef e -> "ref " ^ exp_to_string e
  | EDref e -> "!" ^ exp_to_string e
  | EAssign (e1, e2) -> "(" ^ exp_to_string e1 ^ " := " ^ exp_to_string e2 ^ ")"
  ^ ")"

and binding_to_string2 : binding -> string
= fun (f, is_rec, args, typ, exp) ->
    (let_to_string f) ^ " " ^ args_to_string args "" ^
    (match typ with |TVar _ -> "" |_ -> " : " ^ type_to_string typ) ^
    " = " ^ (lexp_to_string exp) ^ "\n"

let rec decl_to_string2 : decl -> string -> string
= fun decl str ->
  match decl with
  | DExcept ctor -> 
    str ^ "\n" ^ "exception " ^ user_defined_type_to_string ctor ^ "\n"
  | DEqn (x, typ) ->
    str ^ "type " ^ x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    str ^ "type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ user_defined_type_to_string t) lst "") 
    ^ "\n"
  | DLet (f, is_rec, args, typ, exp) -> 
    str ^ "\n" ^ "let " ^ (if is_rec then "rec " else "") ^
    binding_to_string2 (f, is_rec, args, typ, exp)
  | DBlock (is_rec, ds) ->
    str ^ "\n" ^ "" ^ "let " ^ (if is_rec then "rec " else "") ^
    pp_block (binding_to_string2) ds ^ ""
  | TBlock decls ->
    str ^ "\n" ^ "" ^ "type " ^
    pp_block (type_decl_to_string2) decls ^ ""

and type_decl_to_string2 : decl -> string
= fun decl ->
  match decl with
  | DEqn (x, typ) -> x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ user_defined_type_to_string t) lst "") ^ "\n"
  | _ -> raise (Failure "Invalid type decl")

let program_to_string2 : prog -> string
= fun prog -> list_fold decl_to_string2 prog ""

let rec print_pgm2 : prog -> unit
= fun pgm -> print_endline (program_to_string2 pgm)

(* Value Print *)
let string_of_loc = string_of_int

let rec string_of_value ?(env=BatMap.empty) ?(mem=BatMap.empty) : value -> string
= fun v ->
  match v with
  | VUnit -> "()"
  | VInt n -> string_of_int n
  | VBool b -> if b = true then "true" else "false"
  | VString str -> str 
  | VList vs -> pp_list (string_of_value ~env:env ~mem:mem) vs
  | VTuple vs -> pp_tuple (string_of_value ~env:env ~mem:mem) vs
  | VCtor (c, vs) -> c ^ (if vs = [] then "" else pp_tuple (string_of_value ~env:env ~mem:mem) vs)
  | VFun  (xs, _, _) -> "<fun>"
  | VFunRec (f, _, _, _) -> "<fun>"
  | VHole _ -> "?"
  | VBlock (f, vs) -> "" ^ f ^ "|->" ^ pp_block (value_block_to_string ~env:env ~mem:mem) vs ^ ""
  | VRef loc -> 
    if BatMap.is_empty env || BatMap.is_empty mem then "{loc = " ^ string_of_loc loc ^ "}" 
    else "{contents = " ^ string_of_value ~env:env ~mem:mem (lookup_mem loc mem) ^ "}"

and value_block_to_string ?(env=BatMap.empty) ?(mem=BatMap.empty) : id * value -> string
= fun (x, v) -> x ^ " : " ^ string_of_value ~env:env ~mem:mem v

(* Printing env *)
let string_of_env env = BatMap.foldi (fun x v r -> r ^ " " ^ x ^ "|-> " ^ string_of_value v) env ""

let print_env env = print_endline (string_of_env env)

(* Printing mem *)
let string_of_mem mem = BatMap.foldi (fun l v r -> r ^" " ^ string_of_loc l ^ "|-> " ^ string_of_value v) mem ""

let print_mem mem = print_endline (string_of_mem mem)

let print_exp : lexp -> unit
= fun exp -> print_endline (exp_to_string exp)

let print_exp_set : lexp BatSet.t -> unit
= fun exps -> BatSet.iter print_exp exps

let rec print_pgm : prog -> unit
= fun pgm ->
  print_endline (program_to_string pgm)

(*****************************)
(******typechecking***********)
(*****************************)
let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_endline(type_to_string ty1^" = "^type_to_string ty2)) eqns

(*****************************)
(********labeling*************)
(*****************************)
let rec input_to_string : input -> string
= fun es ->
  list_fold (fun e r-> r ^ exp_to_string e ^ " ;" ) es ""

let rec example_to_string : example -> string
= fun (i, o) ->
  (input_to_string i) ^ "=> " ^ (string_of_value o)

let rec print_examples : examples -> unit
= fun examples -> List.iter (
    fun (i,o) -> print_endline (example_to_string (i, o))
  ) examples

let print_parsing_header str = 
  let _ = print_endline "@#$@#$@#$" in
  let _ = print_endline str in
  ()
  
let print_header str = 
 let _ = print_endline "-----------------------------" in
 let _ = print_endline str in
 let _ = print_endline "-----------------------------" in
   ()

let poly_count = ref (Char.code 'a')

let rec pp_polymorphic t env =
  match t with
  |TVar id -> 
    if (BatMap.mem id env) then (BatMap.find id env,env)
    else
      let t = "'"^Char.escaped(Char.chr(!poly_count)) in
      let _ = poly_count:= (!poly_count)+1 in
      (t,BatMap.add id t env) 
  |TList t -> 
    let (str,env) = (pp_polymorphic t env) in
    (str ^ " list",env)
  |TTuple typ_lst -> 
    begin match typ_lst with
    |[] -> ("unit",env)
    |hd::tl -> 
      let (str,env) = pp_polymorphic hd env in
      list_fold (fun t (str,env) ->
        let (s,env) = pp_polymorphic t env in
        (str^" * "^s,env)
      ) tl (str,env)
    end
  |TArr (t1,t2) ->
    let (str,env) = pp_polymorphic t1 env in
    let (str2,env) = pp_polymorphic t2 env in
    ("(" ^ str ^ " -> "^str2^")",env)
  |_ -> (type_to_string t,env)

let print_REPL : prog -> (id, typ) BatMap.t -> env -> mem -> unit
= fun prog tenv env mem ->
  List.iter (fun (decl:decl) ->
    match decl with
    | DLet _ -> ()
    | _ -> print_pgm [decl]
  ) prog;
  let str = BatMap.foldi(fun x v r ->
    let _ = poly_count :=(Char.code 'a') in
    let t = BatMap.find x tenv in
    let (t_string, _) = pp_polymorphic t empty_env in
    ("val " ^ x ^ " : " ^ t_string ^ " = " ^ (string_of_value ~env:env ~mem:mem v) ^ "\n") ^ r
  ) env "" in
  print_string str

(* Symbols *)
let op_to_string : operator -> string
= fun op ->
  match op with
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod"

let cmb_to_string : combinator -> string 
= fun op ->
  match op with
  | And -> "/\\" | Or -> "\\/"

let cmp_to_string : comparator -> string
= fun op ->
  match op with
  | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="

let eq_to_string : eq_operator -> string
= fun op ->
  match op with
  | Eq -> "=" | NEq -> "<>"

let rec symbol_to_string : symbolic_value -> string
= fun sv ->
  match sv with
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Str s -> s
  | List svs -> pp_list symbol_to_string svs
  | Tuple svs -> pp_tuple symbol_to_string svs
  | Ctor (x, svs) -> if svs = [] then x else x ^ " " ^ pp_tuple symbol_to_string svs
  | Exn sv -> "Exception " ^ symbol_to_string sv
  | Symbol n -> "#S (" ^ string_of_int n ^ ")"
  | Fun (x, e, closure) -> "Fun (" ^ arg_to_string x ^ ")"
  | FunRec (f, x, e, closure) -> "FunRec (" ^ f ^ ", " ^ arg_to_string x ^ ")"
  | FunBlock (f, svs) -> "" ^ f ^ "|->" ^ pp_block symbol_block_to_string svs ^ ""
  | Minus sv -> "-(" ^ symbol_to_string sv ^ ")"
  | Not sv -> "not (" ^ symbol_to_string sv ^ ")"
  (* binary operation *)
  | Aop (op, sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ " " ^ op_to_string op ^ " " ^ symbol_to_string sv2 ^ ")"
  | Bop (op, sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ " " ^ cmb_to_string op ^ " " ^ symbol_to_string sv2 ^ ")"
  | ABop (op, sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ " " ^ cmp_to_string op ^ " " ^ symbol_to_string sv2 ^ ")"
  | EQop (op, sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ " " ^ eq_to_string op ^ " " ^ symbol_to_string sv2 ^ ")"
  | Cons (sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ "::" ^ symbol_to_string sv2 ^ ")"
  | Append (sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ "@" ^ symbol_to_string sv2 ^ ")"
  | Strcon (sv1, sv2) -> "(" ^ symbol_to_string sv1 ^ "^" ^ symbol_to_string sv2 ^ ")"

and symbol_block_to_string : id * symbolic_value -> string
= fun (x, sv) -> x ^ " : " ^ symbol_to_string sv

(* Formula *)
let rec aterm_to_string : aterm -> string
= fun t ->
  match t with
  | ASymbol n -> "#A(" ^ string_of_int n ^ ")"
  | Int n -> string_of_int n
  | Op (op, t1, t2) -> "(" ^ aterm_to_string t1 ^ op_to_string op ^ aterm_to_string t2 ^ ")"

let rec sterm_to_string : sterm -> string
= fun t ->
  match t with
  | SSymbol n -> "#S(" ^ string_of_int n ^ ")"
  | Str s -> s

let rec term_to_string : term -> string
= fun t ->
  match t with
  | A t -> aterm_to_string t
  | S t -> sterm_to_string t
  
let rec formula_to_string : formula -> string
= fun f ->
  match f with
  | True -> "true"
  | False -> "false"
  | FSymbol n -> "#F(" ^ string_of_int n ^ ")"
  | Not f -> "not (" ^ formula_to_string f ^ ")"  
  | And (f1, f2) -> "(" ^ formula_to_string f1 ^ "/\\" ^ formula_to_string f2 ^ ")"
  | Or (f1, f2) -> "(" ^ formula_to_string f1 ^ "\\/" ^ formula_to_string f2 ^ ")"
  | Iff (f1, f2) -> "(" ^ formula_to_string f1 ^ "=" ^ formula_to_string f2 ^ ")"
  | Lt (f1, f2) -> "(" ^ aterm_to_string f1 ^ "<" ^ aterm_to_string f2 ^ ")"
  | Le (f1, f2) -> "(" ^ aterm_to_string f1 ^ "<=" ^ aterm_to_string f2 ^ ")"
  | Eq (f1, f2) -> "(" ^ term_to_string f1 ^ "=" ^ term_to_string f2 ^ ")"
