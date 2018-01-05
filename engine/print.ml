open Lang
open Util
open Symbol_lang

(**)

let rec tab_to_string : int -> string
= fun n ->
  "" ^
  match n with
  | 0 -> ""
  | _ -> "  " ^ tab_to_string (n-1)

let rec pat_to_string : pat -> string
= fun pat ->
  match pat with
  |PCtor (x,lst) ->
    begin match lst with
      |[] -> x
      |hd::tl ->
        x ^ "(" ^ pat_to_string hd ^
        (list_fold (fun p r -> r^","^(pat_to_string p)) tl "") ^ ")"
    end
  | Pats lst ->
    list_fold (fun p r -> r ^ "|"^(pat_to_string p)^" ") lst ""
  | PInt n -> string_of_int n
  | PVar x -> x
  | PBool b -> if (b) then "true" else "false"
  | PList lst -> 
    begin match lst with
      |[] -> "[]"
      |hd::tl -> "["^(pat_to_string hd)^
      (list_fold (fun p r -> r^";"^(pat_to_string) p) tl "")^"]"
    end
  | PTuple lst -> 
    begin match lst with
      |[] -> "()"
      |hd::tl ->
        "(" ^ pat_to_string hd ^
        (list_fold (fun p r -> r^","^(pat_to_string p)) tl "") ^ ")"
    end
  | PUnder -> "_"
  | PCons lst ->
    begin match lst with
      |[] -> raise (Failure "Pattern Cons does not have arguments")
      |hd::tl ->
        (pat_to_string hd)^(list_fold (fun p r -> r^"::"^(pat_to_string p)) tl "")
    end

let rec type_to_string : typ -> string
= fun ty -> 
  match ty with
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"
  | TPoly -> "poly"
  | TBase id -> id
  | TList t -> type_to_string t ^ " list"
  | TTuple l -> 
    begin match l with
      | [] -> "unit"
      | hd::tl -> "(" ^ type_to_string hd ^
        (list_fold (fun t r -> r^ "," ^ type_to_string t) tl "")^ ")"
    end
  | TCtor (t, l) -> type_to_string t ^ 
    begin match l with
      | [] -> ""
      | hd::tl -> "(" ^ type_to_string hd ^
        (list_fold (fun t r -> r ^ "," ^ type_to_string t) tl "")^ ")"
    end
  | TArr (t1,t2) -> "(" ^ type_to_string t1 ^ " -> " ^ type_to_string t2 ^ ")"
  | TVar x -> x

let arg_to_string : arg -> string
= fun (x,typ) ->
  match typ with
  |TPoly -> x
  |_ -> "(" ^ x ^ " : " ^ type_to_string typ ^ ")"

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
  |Const n -> string_of_int n
  |String id -> id
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> x
  |EList lst ->
    begin match lst with
    |[] -> "[]"
    |hd::tl ->
      "[" ^ exp_to_string hd ^
      (list_fold (fun t r -> r^";"^ exp_to_string t) tl "") ^ "]"
    end
  |ETuple lst ->
    begin match lst with
    |[] -> "()"
    |hd::tl -> 
      "(" ^ exp_to_string hd ^
      (list_fold (fun t r -> r^","^ exp_to_string t) tl "") ^ ")"
    end
  |ECtor (x,lst) ->  
    begin match lst with
    |[] -> x
    |hd::tl -> 
      x ^ " (" ^ exp_to_string hd ^
      (list_fold (fun t r -> r^","^ exp_to_string t) tl "") ^ ")"
    end
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
      (if t=TPoly then "" else " : " ^ type_to_string t) ^
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

let rec decl_to_string : decl -> string -> string
= fun decl str ->
  match decl with
  | DData (id,lst) -> 
    str ^ "type " ^ id ^ " =" ^ 
    (list_fold (fun t r -> r ^ "\n|" ^ user_defined_type_to_string t) lst "") ^ "\n"
  | DLet (x,is_rec,args,typ,exp) -> 
    match args with
    | [] -> (* variable binding *)
      str ^ "\n" ^ "let " ^
      (if(is_rec) then "rec " else "" ) ^
      x ^ " = "^(exp_to_string exp) ^ ";;\n"
    | _ ->  (* function binding *)
      let args_string = args_to_string args "" in
      str ^ "\n" ^ "let " ^
      (if(is_rec) then "rec " else "") ^ x ^" " ^args_string ^
      (if(typ=TPoly) then "" else " : " ^ type_to_string typ) ^
      " = " ^ (exp_to_string exp) ^ ";;\n"

let program_to_string : prog -> string
= fun prog -> list_fold decl_to_string prog ""

let rec value_to_string : value -> string
= fun v ->
  match v with
  | VInt n1 -> string_of_int n1
  | VBool b1 -> if b1 = true then "true" else "false"
  | VString str -> str 
  | VList l1 -> if (l1=[]) then "list []" else "list [" ^ string_of_list l1 ^ "]"
  | VTuple l1 -> "Tuple (" ^ string_of_tuple l1 ^ ")"
  | VCtor (id,l1) -> "VCtor " ^ id ^ "(" ^ string_of_tuple l1 ^ ")"
  | VFun  (id,exp,env) -> "VFun " ^ id  ^ " [env" ^ env_to_string env ^"]"
  | VFunRec (id1,id2,exp,env) -> "VFunRec " ^ id1 ^ " " ^ id2 ^ " [env" ^ env_to_string env ^"]"
  | _ -> "?"

and string_of_list l1 =
  match l1 with
  | [] -> "\b"
  | hd::tl -> (value_to_string hd) ^ ";" ^ (string_of_list tl)

and string_of_tuple l1 =
  match l1 with
  | [] -> "\b"
  | hd::tl -> (value_to_string hd) ^ "," ^ (string_of_tuple tl)

and env_to_string env =
  if (BatMap.is_empty env) then ""
  else(
    let ((x,v),env1) = BatMap.pop env in " " ^ x ^ "|-> " ^ value_to_string v ^ env_to_string env1
  )

and fun_to_string l1 =
  match l1 with
  | [] -> " "
  | hd::tl -> let (v1,v2) = hd in "(" ^ value_to_string v1 ^ "," ^ value_to_string v2 ^ ")"^ fun_to_string tl 

and printEnv : env -> unit
  =fun env -> 
      BatMap.iter (fun x v -> print_endline (x ^ " |-> " ^ value_to_string v)) env

let print_exp : exp -> unit
= fun exp -> print_endline (exp_to_string exp)

let rec print_exp_set : exp BatSet.t -> unit
= fun exps ->
  if (BatSet.is_empty exps) then ()
  else
    let (exp,exps) = BatSet.pop exps in
    let _ = print_exp exp in
    print_exp_set exps

let print_pat : pat -> unit
= fun pat -> print_endline (pat_to_string pat)

let rec print_pat_set : pat BatSet.t -> unit
= fun pats ->
  if (BatSet.is_empty pats) then ()
  else
    let (pat,pats) = BatSet.pop pats in
    let _ = print_pat pat in
    print_pat_set pats



let rec print_pgm : prog -> unit
= fun pgm ->
  print_endline (program_to_string pgm)

let print_component : components -> unit
= fun exp_set ->
  print_endline ("-----------------------------");
  print_endline ("expression component set is below");
  print_endline ("-----------------------------");
  print_exp_set exp_set
  

let rec print_examples : examples -> unit
= fun examples ->
  match examples with
  |[] -> ()
  |(exp,value)::tl -> 
  (
    let str_value = (value_to_string value) in
    let rec f l =
    match l with
    [] -> ""
    |hd::tl -> exp_to_string hd ^ " " ^ f tl in
    let str_exp = f exp in
    let _ =  (print_string (str_exp ^ " -> ")) in
    let _ = print_endline str_value in
    print_examples tl
  )

let rec labeled_exp_to_string : labeled_exp -> string
= fun (n,exp) ->
  "( " ^ (string_of_int n) ^ ", "^ 
  begin match exp with
  |Const n -> string_of_int n
  |TRUE -> "true"
  |FALSE -> "false"
  |EVar x -> x
  |EList lst ->
    if (lst=[]) then "[]" else
    let rec f lst =
    match lst with
    |[] -> "\b]"
    |hd::tl -> labeled_exp_to_string hd ^ ";" ^ (f tl)
  in "[" ^ f lst
  |ETuple lst ->
    if (lst=[]) then "()" else
    let rec f lst =
    match lst with
    |[] -> "\b)"
    |hd::tl -> labeled_exp_to_string hd ^ "," ^ (f tl)
  in "(" ^ f lst
  |ECtor (x,lst) ->
    let rec f lst =
    match lst with
    |[] -> ")"
    |hd::tl -> labeled_exp_to_string hd ^ "," ^ (f tl)
  in x^" (" ^ f lst
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
  |EFun ((id,t),e1) -> "fun " ^ id ^ " -> " ^ labeled_exp_to_string e1 
  |Hole n -> "?"
  |EMatch (e,lst) ->  
    let rec f lst =
    match lst with
    |[] -> ""
    |(pat,exp)::tl -> "\n|" ^ labeled_pat_to_string pat ^ " -> " ^ labeled_exp_to_string exp ^ (f tl)
  in "match " ^ labeled_exp_to_string e ^ " with " ^ f lst
  |_ -> ""
  end ^ ")"


and labeled_pat_to_string : labeled_pat -> string
= fun (n,pat) ->
  "( " ^ (string_of_int n) ^ ", "^ 
  begin match pat with
  PCtor (x,lst) ->
    let rec f lst =
    match lst with
    |[] -> ")"
    |hd::tl -> labeled_pat_to_string hd ^ "," ^ (f tl)
  in x ^ "(" ^ f lst
  | Pats lst ->
    let rec f lst =
    match lst with
    |[] -> ""
    |hd::tl -> labeled_pat_to_string hd ^ "|" ^ (f tl)
  in " " ^ f lst
  | PInt n -> string_of_int n
  | PVar x -> x
  | PBool b -> if (b) then "true" else "false"
  | PList lst -> 
    let rec f lst =
    match lst with
    |[] -> "]"
    |hd::tl -> labeled_pat_to_string hd ^ ";" ^ (f tl)
  in "[" ^ f lst
  | PTuple lst -> 
    let rec f lst =
    match lst with
    |[] -> ")"
    |hd::tl -> labeled_pat_to_string hd ^ "," ^ (f tl)
  in "PTuple (" ^ f lst
  | PUnder -> "_"
  | PCons lst ->
    let rec f lst =
    match lst with
    |[] -> "\b\b"
    |hd::tl -> labeled_pat_to_string hd ^ "::" ^ (f tl)
  in (f lst)
 end ^ ")"
let rec labeled_decl_to_string : labeled_decl -> string -> string
= fun decl str ->
  match decl with
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
        if (typ=TPoly) then "\b\b\b" 
        else type_to_string typ 
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
  | VInt n -> (string_of_int n)
  | VBool b -> if b then "true" else "false"
  | VList vs -> "[" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ ";" ^ str) vs "") ^"]" 
  | VTuple vs -> "(" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ "," ^ str) vs "") ^")" 
  | VCtor (x, vs) -> x ^ "(" ^ (list_fold (fun x str -> (labeled_value_to_string x) ^ "," ^ str) vs "") ^")" 
  | VFun (x, exp, lenv, senv) -> "VFun" ^ x
  | VFunRec (f, x, exp, lenv, senv) -> "VFun" ^ f ^ x
  | VHole n -> "?"

let rec string_of_symbol : symbolic_value -> string
= fun symbol ->
  match symbol with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Str x -> x
  | Symbol n -> "e" ^ string_of_int n
  | List es -> 
    let rec f lst =
      match lst with
      |[] -> ""
      |[a] -> string_of_symbol a
      |hd::tl -> string_of_symbol hd ^ ";" ^ (f tl)
    in
    "[" ^ f es ^ "]"
  | Tuple es ->
    let rec f lst =
      match lst with
      |[] -> ""
      |[a] -> string_of_symbol a
      |hd::tl -> string_of_symbol hd ^ "," ^ (f tl)
    in
    "(" ^ f es ^ ")"
  | Ctor (x, es) ->
    let rec f lst =
      match lst with
      |[] -> ""
      |[a] -> string_of_symbol a
      |hd::tl -> string_of_symbol hd ^ "," ^ (f tl)
    in
    x ^ " (" ^ f es ^ ")"
  | Fun (x, e, closure) -> "Fun " ^ x
  | FunRec (f, x, e, closure) -> "FunRec " ^ f ^ " " ^ x
  | Add (e1, e2) -> "(" ^ string_of_symbol e1 ^ " + " ^ string_of_symbol e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_symbol e1 ^ " - " ^ string_of_symbol e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_symbol e1 ^ " * " ^ string_of_symbol e2 ^ ")"
  | Div (e1, e2) -> "(" ^ string_of_symbol e1 ^ " / " ^ string_of_symbol e2 ^ ")"
  | Mod (e1, e2) -> "(" ^ string_of_symbol e1 ^ " % " ^ string_of_symbol e2 ^ ")"
  | Minus e -> " - " ^ "(" ^ string_of_symbol e ^ ")"
  | Not e -> " not " ^ "(" ^ string_of_symbol e ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_symbol e1 ^ " || " ^ string_of_symbol e2 ^ ")"
  | And (e1, e2) -> "(" ^ string_of_symbol e1 ^ " && " ^ string_of_symbol e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ string_of_symbol e1 ^ " < " ^ string_of_symbol e2 ^ ")"
  | Gt (e1, e2) -> "(" ^ string_of_symbol e1 ^ " > " ^ string_of_symbol e2 ^ ")"
  | Le (e1, e2) -> "(" ^ string_of_symbol e1 ^ " <= " ^ string_of_symbol e2 ^ ")"
  | Ge (e1, e2) -> "(" ^ string_of_symbol e1 ^ " >= " ^ string_of_symbol e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_symbol e1 ^ " = " ^ string_of_symbol e2 ^ ")"
  | NEq (e1, e2) -> "(" ^ string_of_symbol e1 ^ " != " ^ string_of_symbol e2 ^ ")"
  | At (e1, e2) -> string_of_symbol e1 ^ " @ " ^ string_of_symbol e2
  | Cons (e1, e2) -> string_of_symbol e1 ^ " :: " ^ string_of_symbol e2
  | If (e1, e2, e3) -> "if (" ^ string_of_symbol e1 ^ ", " ^ string_of_symbol e2 ^ ", " ^ string_of_symbol e3 ^ ")"

let print_header str = 
 let _ = print_endline "-----------------------------" in
 let _ = print_endline str in
 let _ = print_endline "-----------------------------" in
   ()
