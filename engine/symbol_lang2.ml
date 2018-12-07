open Lang
open Util

(* equality *)
type eq_operator =
  | Eq
  | NEq

(* bop *)
type combinator =
  | And
  | Or 

(* abbop *)
type comparator =
  | Lt
  | Gt
  | Le
  | Ge 

(* abop *)
type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

(* Symbolic Execution *)
type symbolic_value = 
  (* Const *)
  | Unit
  | Exn
  | Int of int
  | Bool of bool
  | Str of string
  | List of symbolic_value list 
  | Tuple of symbolic_value list
  | Ctor of id * symbolic_value list
  | Symbol of int
  | Fun of arg * lexp * symbolic_env
  | FunRec of id * arg * lexp * symbolic_env * depth
  | FunBlock of id * (id * sym_formula) list
  (* unary operation *)
  | Minus of symbolic_value
  | Not of symbolic_value
  (* binary operation *)
  | Aop of operator * symbolic_value * symbolic_value
  | Bop of combinator * symbolic_value * symbolic_value
  | ABop of comparator * symbolic_value * symbolic_value
  | EQop of eq_operator * symbolic_value * symbolic_value
  | Cons of symbolic_value * symbolic_value
  | Append of symbolic_value * symbolic_value
  | Strcon of symbolic_value * symbolic_value
(* variable to symbolic variable ex. x -> e1 *)
and path_cond = (symbolic_value) BatSet.t 
and sym_state = (path_cond * symbolic_value)
and sym_formula = sym_state BatSet.t
and symbolic_env = (id, sym_formula) BatMap.t
and depth = int

(* Verification Condition *)
type vc_formula = (symbolic_value * symbolic_value) list (* \/ [pc_c => pc_b /\ sv = sv] *)
type vc = vc_formula list (* /\ (pc_c => \/ (pc_b /\ sv = sv))*)

let init_pc = BatSet.empty
let gen_pc pc = BatSet.singleton pc
let extend_pc pc1 pc2 = BatSet.union pc1 pc2

let rec is_exn : symbolic_value -> bool
= fun sv ->
  match sv with 
  | Exn -> true
  | List svs | Tuple svs | Ctor (_, svs) -> List.exists is_exn svs
  | Minus sv | Not sv -> is_exn sv
  | Aop (_, sv1, sv2) | Bop (_, sv1, sv2) | ABop (_, sv1, sv2) | EQop (_, sv1, sv2) | Cons (sv1, sv2) | Append (sv1, sv2) | Strcon (sv1, sv2) -> (is_exn sv1) || (is_exn sv2)
  | _ -> false

let rec flatten_pc : path_cond -> symbolic_value
= fun pc -> BatSet.fold (fun pc acc -> Bop (And, pc, acc)) pc (Bool true)

let rec symval_to_pc : symbolic_value -> path_cond
= fun sv ->
  match sv with
  | Bop (And, sv1, sv2) -> BatSet.union (symval_to_pc sv1) (symval_to_pc sv2)
  | _ -> BatSet.singleton sv

(* PP *)
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
  | List svs -> Print.pp_list symbol_to_string svs
  | Tuple svs -> Print.pp_tuple symbol_to_string svs
  | Ctor (x, svs) -> if svs = [] then x else x ^ " " ^ Print.pp_tuple symbol_to_string svs
  | Exn -> "Exception"
  | Symbol n -> "#A (" ^ string_of_int n ^ ")"
  | Fun (x, e, closure) -> "Fun (" ^ Print.arg_to_string x ^ ")"
  | FunRec (f, x, e, closure, k) -> "FunRec (" ^ f ^ ", " ^ Print.arg_to_string x ^ ") : " ^ string_of_int k 
  | FunBlock (f, svs) -> "{" ^ f ^ "|->" ^ Print.pp_block symbol_block_to_string svs ^ "}"
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

and symbol_block_to_string : id * sym_formula -> string
= fun (x, psi) -> x ^ BatSet.fold (fun (pc, sv) acc ->
    acc ^ symbol_to_string sv ^ ", "
  ) psi ""

and string_of_pc : path_cond -> string
= fun pc -> BatSet.fold (fun pc' str -> (symbol_to_string pc' ^ " /\\ ") ^ str) pc ""

let print : sym_formula -> unit
= fun psi ->
  BatSet.iter (fun (pc, sv) ->
    print_endline ("PC : \n" ^ string_of_pc pc);
    print_endline ("SV : " ^ symbol_to_string sv);
    print_endline ("\\/")
  ) psi
