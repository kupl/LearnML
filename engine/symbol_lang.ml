open Lang

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

(************* Partial Evaluation **************)
type symbolic_value = 
  (* Const *)
  | Unit
  | Int of int
  | Bool of bool
  | Str of string
  | List of symbolic_value list 
  | Tuple of symbolic_value list
  | Ctor of id * symbolic_value list
  | Exn of symbolic_value
  | Symbol of int
  | Fun of arg * lexp * symbolic_env
  | FunRec of id * arg * lexp * symbolic_env
  | FunBlock of id * (id * symbolic_value) list
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
and symbolic_env = (id, symbolic_value) BatMap.t

(* new symbol *)
let symbol_num = ref 0
let init_symbol () = symbol_num := 0
let fresh_symbol () = (symbol_num := !symbol_num + 1; (Symbol (!symbol_num)))

(************* SMT formula **************)
(* Int *)
type aterm =
  | ASymbol of int
  | Int of int
  | Op of operator * aterm * aterm

(* String => char *)
type sterm =
  | SSymbol of int
  | Str of id

type term =
  (* Int *)
  | A of aterm
  (* String *) 
  | S of sterm 

(* Bool domain*)
type formula =
  | True
  | False
  | FSymbol of int
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Iff of formula * formula
  | Lt of aterm * aterm
  | Le of aterm * aterm
  | Eq of term * term