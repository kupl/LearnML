open Lang
open Util
open Label_lang

(*****************************)
(********language*************)
(*****************************)

let rec tab_to_string : int -> string
= fun n ->
  "" ^
  match n with
  | 0 -> ""
  | _ -> "  " ^ tab_to_string (n-1)

let pp_tuple func tup =
  match tup with
  |[] -> "()"
  |hd::tl -> 
    "(" ^ (func hd) ^
    (list_fold (fun elem r -> r ^ ", " ^ (func elem)) tl "") ^ ")"

let pp_list func lst =
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
  | PCons (lst) ->
    begin match lst with
      |[] -> raise (Failure "Pattern Cons does not have arguments")
      |hd::tl ->
        (pat_to_string hd)^(list_fold (fun p r -> r^"::"^(pat_to_string p)) tl "")
    end
  (*| PCons (phd, ptl) -> (pat_to_string phd) ^ "::" ^ (pat_to_string) *)

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

let rec arg_to_string : arg -> string
= fun arg ->
  match arg with
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

let rec exp_to_string : exp -> string
= fun exp ->
  match exp with
  |EUnit -> "()"
  |Const n -> string_of_int n
  |String id -> id
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> x
  |EList lst -> pp_list exp_to_string lst
  |ETuple lst -> pp_tuple exp_to_string lst
  |ECtor (x,lst) -> x ^ (if lst=[] then "" else " " ^ exp_to_string (List.hd lst))
  |ADD (e1,e2) -> "(" ^ exp_to_string e1 ^ " + " ^ exp_to_string e2 ^")"  
  |SUB (e1,e2) -> "(" ^ exp_to_string e1 ^ " - " ^ exp_to_string e2 ^")"  
  |MUL (e1,e2) -> "(" ^ exp_to_string e1 ^ " * " ^ exp_to_string e2 ^")"  
  |DIV (e1,e2) -> "(" ^ exp_to_string e1 ^ " / " ^ exp_to_string e2 ^")"  
  |MOD (e1,e2) -> "(" ^ exp_to_string e1 ^ " % " ^ exp_to_string e2 ^")"  
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
  |NOT e -> "not (" ^ exp_to_string e ^ ")"
  |EApp (e1,e2) -> 
    exp_to_string e1 ^ " " ^
    begin match e2 with
    |EApp _ -> "(" ^ exp_to_string e2 ^ ")"
    |_ -> exp_to_string e2
    end
  |IF (e1,e2,e3) -> 
    "if " ^ exp_to_string e1 ^ 
    " then " ^ exp_to_string e2 ^ "\n" ^
    " else " ^ exp_to_string e3
  |ELet (f,is_rec,xs,t,e1,e2) -> 
    begin match xs with
    | [] -> (* variable binding *)
      "\n" ^ "let " ^ 
      (if is_rec then "rec " else "") ^
      f ^ " = "^(exp_to_string e1) ^ " in \n" ^ (exp_to_string e2)
    | _ ->  (* function binding *)
      let args_string = args_to_string xs "" in
      "\n" ^ "let " ^
      (if is_rec then "rec " else "") ^ f ^" "^ args_string ^
      (match t with |TVar _ -> "" |_ -> " : " ^ type_to_string t) ^
      " = " ^ (exp_to_string e1) ^ " in \n" ^ (exp_to_string e2)
    end
  |EFun (arg,e1) -> 
    let rec multi_args (e : exp) r =
      begin match e with
      |EFun (a,exp) -> 
        let arg_seq = r ^ " " ^ arg_to_string a in
        multi_args exp arg_seq
      |_ -> "\nfun " ^ r ^ " -> " ^ exp_to_string e
    end in
    multi_args e1 (arg_to_string arg) 
  |EMatch (e,lst) ->  
    "\nmatch " ^ exp_to_string e ^ " with " ^ 
    (list_fold (fun (p,e) r -> r ^ "\n|" ^ pat_to_string p ^ " -> " ^ exp_to_string e) lst "")
  |Hole n -> "?"
  |Raise e -> "raise "^exp_to_string e

let rec decl_to_string : decl -> string -> string
= fun decl str ->
  match decl with
  | DExcept ctor -> 
    str ^ "exception " ^ user_defined_type_to_string ctor
  | DEqn (x, typ) ->
    str ^ "type " ^ x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    str ^ "type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ user_defined_type_to_string t) lst "") ^ "\n"
  | DLet (x,is_rec,args,typ,exp) -> 
    match args with
    | [] -> (* variable binding *)
      str ^ "\n" ^ "let " ^
      (if(is_rec) then "rec " else "" ) ^
      x ^ 
      (match typ with |TVar _ -> "" |_ -> " : " ^ type_to_string typ) ^
      " = "^(exp_to_string exp) ^ "\n"
    | _ ->  (* function binding *)
      let args_string = args_to_string args "" in
      str ^ "\n" ^ "let " ^
      (if(is_rec) then "rec " else "") ^ x ^" " ^args_string ^
      (match typ with |TVar _ -> "" |_ -> " : " ^ type_to_string typ) ^
      " = " ^ (exp_to_string exp) ^ "\n"

let program_to_string : prog -> string
= fun prog -> list_fold decl_to_string prog ""

let rec value_to_string : value -> string
= fun v ->
  match v with
  | VUnit -> "()"
  | VInt n1 -> string_of_int n1
  | VBool b1 -> if b1 = true then "true" else "false"
  | VString str -> str 
  | VList l1 -> pp_list value_to_string l1
  | VTuple l1 -> pp_tuple value_to_string l1
  | VCtor (id,l1) -> id ^ (if(l1=[]) then "" else pp_tuple value_to_string l1)
  | VFun  (xs, exp, env) -> "fun "^ arg_to_string xs ^"->"^(exp_to_string exp)
  | VFunRec (f, xs, exp, env) -> "VFunRec ("^f^","^  arg_to_string xs ^","^exp_to_string exp^",env)" 
  | VHole _ -> "?"
  
let env_to_string env = BatMap.foldi (fun x v r -> r^" " ^x ^ "|-> " ^ value_to_string v) env ""

let print_env : env -> unit
= fun env -> print_endline (env_to_string env)

let print_exp : exp -> unit
= fun exp -> print_endline (exp_to_string exp)

let print_exp_set : exp BatSet.t -> unit
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
let rec print_examples : examples -> unit
= fun examples -> List.iter 
  (
    fun (i,o) -> 
      let input_string = list_fold (fun e r-> r ^ exp_to_string e ^ " " ) i "" in
      let output_string = value_to_string o in
      print_endline(input_string ^ "-> "^output_string)
  ) examples

let rec labeled_exp_to_string : labeled_exp -> string
= fun (n,exp) ->
  "( " ^ (string_of_int n) ^ ", "^ 
  begin match exp with
  | EUnit -> "()"
  |Const n -> string_of_int n
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> x
  |EList lst -> pp_list labeled_exp_to_string lst
  |ETuple lst -> pp_tuple labeled_exp_to_string lst
  |ECtor (x,lst) -> x ^ (if lst=[] then "" else pp_tuple labeled_exp_to_string lst)
  |ADD (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " + " ^ labeled_exp_to_string e2 ^")"  
  |SUB (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " - " ^ labeled_exp_to_string e2 ^")"  
  |MUL (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " * " ^ labeled_exp_to_string e2 ^")"  
  |DIV (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " / " ^ labeled_exp_to_string e2 ^")"  
  |MOD (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " % " ^ labeled_exp_to_string e2 ^")"  
  |MINUS (e) -> "(-" ^ labeled_exp_to_string e ^ ")"
  |OR (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " || " ^ labeled_exp_to_string e2 ^")"
  |AND (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " && " ^ labeled_exp_to_string e2 ^")"
  |LESS (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " < " ^ labeled_exp_to_string e2 ^")"
  |LARGER (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " > " ^ labeled_exp_to_string e2 ^")"
  |LARGEREQ (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " >= " ^ labeled_exp_to_string e2 ^")"
  |EQUAL (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " = " ^ labeled_exp_to_string e2 ^")"
  |NOTEQ (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " != " ^ labeled_exp_to_string e2 ^")"
  |LESSEQ (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " <= " ^ labeled_exp_to_string e2 ^")"
  |AT (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " @ " ^ labeled_exp_to_string e2 ^")"
  |DOUBLECOLON (e1,e2) -> "(" ^ labeled_exp_to_string e1 ^ " :: " ^ labeled_exp_to_string e2 ^")"
  |NOT e -> "not (" ^ labeled_exp_to_string e ^ ")"
  |EApp (e1,e2) -> labeled_exp_to_string e1 ^ " " ^ labeled_exp_to_string e2
  |IF (e1,e2,e3) -> 
    "if " ^ labeled_exp_to_string e1 ^ "" ^ 
    " then " ^ labeled_exp_to_string e2 ^ "\n" ^
    " else " ^ labeled_exp_to_string e3 ^ ""
  |ELet (f,is_rec,xs,t,e1,e2) -> 
    begin match xs with
    | [] -> (* variable binding *)
      "\n" ^ "let " ^ f ^ " = "^(labeled_exp_to_string e1) ^ " in " ^ (labeled_exp_to_string e2)
    | _ ->  (* function binding *)
      let args_string = args_to_string xs "" in
      let typ_string = type_to_string t in
      if is_rec then
        "\n" ^ "let rec " ^ f ^ args_string ^ " : " ^ typ_string ^ " = \n" ^(labeled_exp_to_string e1) ^ " in " ^ (labeled_exp_to_string e2)
      else
        "\n" ^ "let " ^ f ^ args_string ^ " : " ^ typ_string ^ " = \n" ^(labeled_exp_to_string e1) ^ " in " ^ (labeled_exp_to_string e2)
    end
  |EFun (arg ,e1) -> "fun " ^ (arg_to_string arg) ^ " -> " ^ labeled_exp_to_string e1 
  |Hole n -> "?"
  |EMatch (e,lst) ->  
    let rec f lst =
    match lst with
    |[] -> ""
    |(pat,exp)::tl -> "\n|" ^ pat_to_string pat ^ " -> " ^ labeled_exp_to_string exp ^ (f tl)
  in "match " ^ labeled_exp_to_string e ^ " with " ^ f lst
  |_ -> ""
  end ^ ")"

let rec labeled_decl_to_string : labeled_decl -> string -> string
= fun decl str ->
  match decl with
  | DExcept t ->
    str ^ "exception " ^ user_defined_type_to_string t
  | DEqn (x, typ) ->
    str ^ "type " ^ x ^ " = " ^ type_to_string typ ^ "\n"
  | DData (id,lst) -> 
    str ^ "type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ user_defined_type_to_string t) lst "") ^ "\n"
  | DLet (x,is_rec,args,typ,exp) -> 
    match args with
    | [] -> (* variable binding *)
      str ^ "\n" ^ "let " ^ x ^ " = "^(labeled_exp_to_string exp) ^ ";;\n"
    | _ ->  (* function binding *)
      let args_string = args_to_string args "" in
      let typ_string = 
        type_to_string typ 
      in
      if is_rec then
        str ^ "\n" ^ "let rec " ^ x ^ args_string ^ " : " ^ typ_string ^ " = \n" ^(labeled_exp_to_string exp) ^ ";;\n"
      else
        str ^ "\n" ^ "let " ^ x ^ args_string ^ " : " ^ typ_string ^ " = \n" ^(labeled_exp_to_string exp) ^ ";;\n"

let labeled_program_to_string : labeled_prog -> string
= fun pgm ->
  list_fold labeled_decl_to_string pgm "" 

let print_labeled_pgm : labeled_prog -> unit
= fun pgm ->
  print_endline (labeled_program_to_string pgm)

let rec labeled_value_to_string : labeled_value -> string
= fun v ->
  match v with
  | VUnit -> "()"
  | VInt n -> (string_of_int n)
  | VBool b -> if b then "true" else "false"
  | VString str -> str
  | VList vs -> "[" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ ";" ^ str) vs "") ^"]" 
  | VTuple vs -> "(" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ "," ^ str) vs "") ^")" 
  | VCtor (x, vs) -> x ^ "(" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ "," ^ str) vs "") ^")" 
  | VFun (arg, exp, lenv) -> "VFun" ^ arg_to_string arg
  | VFunRec (f, arg, exp, lenv) -> "VFun" ^ f ^ arg_to_string arg
  | VHole n -> "?"

let print_header str = 
 let _ = print_endline "-----------------------------" in
 let _ = print_endline str in
 let _ = print_endline "-----------------------------" in
   ()
