open Lang
open Util
open CallGraph

(*********************************************************************************)
(* Compute syntactic distance using 1-level position-aware characteristic vector *)
(*********************************************************************************)

(* atomic pattern identifier -> [sequence of tree height where the atomic pattern appears] *)
type vector = (string, int list) BatMap.t

let update_vec vec key value = 
	if BatMap.mem key vec then BatMap.modify key (fun seq -> value::seq) vec
	else BatMap.add key [value] vec

(* Atomic tree pattern : exp *)
let rec vectorize : vector -> int -> lexp -> vector
= fun vec d (l, exp) ->
	match exp with
	| EUnit -> update_vec vec "unit" d
	| Const _ -> update_vec vec "int" d
	| TRUE -> update_vec vec "true" d
	| FALSE -> update_vec vec "false" d
	| String _ -> update_vec vec "string" d
	| EVar _ -> update_vec vec "var" d
  | EFun (arg, e) -> vectorize (update_vec vec "fun" d) (d + 1) e
  | ERef e -> vectorize (update_vec vec "ref" d) (d + 1) e
  | EDref e -> vectorize (update_vec vec "dref" d) (d + 1) e
  | Raise e -> vectorize (update_vec vec "raise" d) (d + 1) e
  | MINUS e -> vectorize (update_vec vec "minus" d) (d + 1) e
  | NOT e -> vectorize (update_vec vec "not" d) (d + 1) e
  | ADD (e1, e2) -> vectorize (vectorize (update_vec vec "add" d) (d + 1) e1) (d + 1) e2
  | SUB (e1, e2) -> vectorize (vectorize (update_vec vec "sub" d) (d + 1) e1) (d + 1) e2
  | MUL (e1, e2) -> vectorize (vectorize (update_vec vec "mul" d) (d + 1) e1) (d + 1) e2
  | DIV (e1, e2) -> vectorize (vectorize (update_vec vec "div" d) (d + 1) e1) (d + 1) e2
  | MOD (e1, e2) -> vectorize (vectorize (update_vec vec "mod" d) (d + 1) e1) (d + 1) e2
  | OR (e1, e2) -> vectorize (vectorize (update_vec vec "or" d) (d + 1) e1) (d + 1) e2
  | AND (e1, e2) -> vectorize (vectorize (update_vec vec "and" d) (d + 1) e1) (d + 1) e2
  | LESS (e1, e2) -> vectorize (vectorize (update_vec vec "lt" d) (d + 1) e1) (d + 1) e2
  | LESSEQ (e1, e2) -> vectorize (vectorize (update_vec vec "le" d) (d + 1) e1) (d + 1) e2
  | LARGER (e1, e2) -> vectorize (vectorize (update_vec vec "gt" d) (d + 1) e1) (d + 1) e2
  | LARGEREQ (e1, e2) -> vectorize (vectorize (update_vec vec "ge" d) (d + 1) e1) (d + 1) e2
  | EQUAL (e1, e2) -> vectorize (vectorize (update_vec vec "eq" d) (d + 1) e1) (d + 1) e2
  | NOTEQ (e1, e2) -> vectorize (vectorize (update_vec vec "neq" d) (d + 1) e1) (d + 1) e2
  | DOUBLECOLON (e1, e2) -> vectorize (vectorize (update_vec vec "cons" d) (d + 1) e1) (d + 1) e2 
  | AT (e1, e2) -> vectorize (vectorize (update_vec vec "app" d) (d + 1) e1) (d + 1) e2 
  | STRCON (e1, e2) -> vectorize (vectorize (update_vec vec "strcons" d) (d + 1) e1) (d + 1) e2
  | EApp (e1, e2) -> vectorize (vectorize (update_vec vec "call" d) (d + 1) e1) (d + 1) e2
  | EAssign (e1, e2) -> vectorize (vectorize (update_vec vec "assign" d) (d + 1) e1) (d + 1) e2
  | EList es -> List.fold_left (fun acc e -> vectorize acc (d + 1) e) (update_vec vec "list" d) es
  | ETuple es -> List.fold_left (fun acc e -> vectorize acc (d + 1) e) (update_vec vec "tuple" d) es
  | ECtor (x, es) -> List.fold_left (fun acc e -> vectorize acc (d + 1) e) (update_vec vec "list" d) es
  | IF (e1, e2, e3) -> 
  	vectorize (vectorize (vectorize (update_vec vec "if" d) (d + 1) e1) (d + 1) e2) (d + 1) e3
  | EMatch (e, bs) -> 
  	List.fold_left (fun acc (_, e) -> vectorize acc (d + 1) e) (vectorize (update_vec vec "match" d) (d + 1) e) bs
  | ELet (f, is_rec, args, typ, e1, e2) -> vectorize (vectorize (update_vec vec "let" d) (d + 1) e1) (d + 1) e2
  | EBlock (is_rec, bindings, e) -> 
  	List.fold_left (fun acc (_, _, _, _, e) -> vectorize acc (d + 1) e) (vectorize (update_vec vec "letand" d) (d + 1) e) bindings
  | _ -> raise (Failure ("Vector extraction : invalid exp (" ^ Print.exp_to_string (l, exp)))
 
let rec compute_norm2 : int list -> int list -> float 
= fun seq1 seq2 ->	
	match (seq1, seq2) with
	| [], [] -> 0.
	| hd1::tl1, hd2::tl2 -> (float_of_int ((hd1 - hd2) * (hd1 - hd2))) +. compute_norm2 tl1 tl2
	| lst, [] | [], lst -> float_of_int (List.fold_left (fun acc e -> acc + (e * e)) 0 lst)

let rec compute_dist : vector -> vector -> float 
= fun vec1 vec2 ->
	BatMap.foldi (fun key seq1 acc ->
		let dist = 
			if BatMap.mem key vec2 then
				sqrt (compute_norm2 (List.sort compare seq1) (List.sort compare (BatMap.find key vec2)))
			else sqrt (compute_norm2 (List.sort compare seq1) [])
		in
		dist +. acc
	) vec1 0.

let rec syntactic_distance : lexp -> lexp -> float
= fun e1 e2 ->
	let (vec1, vec2) = (vectorize BatMap.empty 0 e1, vectorize BatMap.empty 0 e2) in
	compute_dist vec1 vec2

