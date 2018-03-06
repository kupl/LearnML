open Lang
open Util
open Symbol_lang
open Translate

open Aez
open Smt

module T = Term
module F = Formula
module H = Hstring
module Solver = Make (struct end)

module Cache = struct
  (* program string -> bool*)
  type t = (string, bool) BatMap.t  
  let empty = BatMap.empty 
  let mem = BatMap.mem
  let find = BatMap.find
  let add = BatMap.add
  let print t = BatMap.iter (fun p b -> print_endline (p ^ " -> " ^ if b then "SAT" else "UNSAT")) t
end

module Converter = struct
  (* Convert the inductive symbolic values to more concise form *)

  (* a symbol -> symbol value *)
  type t = (int, symbolic_value) BatMap.t (* is global?? *)

  let empty = BatMap.empty
  let mem = BatMap.mem
  let find = BatMap.find
  let extend = BatMap.add

  let rec connect_formula : formula list -> formula
  = fun fs ->
    match fs with
    | [] -> True
    | hd::tl -> And (hd, connect_formula tl)

  (* Symbolic value -> Int *)
  let rec symbol_to_aterm : symbolic_value -> aterm
  = fun sv ->
    match sv with
    | Int n -> Int n
    | Symbol n -> ASymbol n
    | Minus sv -> Op (Sub, symbol_to_aterm ((Int 0 : symbolic_value)), symbol_to_aterm sv)
    | Aop (op, sv1, sv2) -> Op (op, symbol_to_aterm sv1, symbol_to_aterm sv2)
    | _ -> raise (Failure "Invalid ATerm")

  (* Symbolic value -> String *)
  let rec symbol_to_sterm : symbolic_value -> sterm
  = fun sv ->
    match sv with
    | Str s -> Str s
    | Symbol n -> SSymbol n
    | _ -> raise (Failure "Invalid STerm")

  (* Symbolic value -> Bool *)
  let rec symbol_to_formula : symbolic_value -> formula
  = fun sv ->
    match sv with
    | Bool b -> if b then True else False
    | Symbol n -> FSymbol n
    | Not sv -> Not (symbol_to_formula sv)
    | Bop (op, sv1, sv2) ->
      begin match op with
      | And -> And (symbol_to_formula sv1, symbol_to_formula sv2)
      | Or -> Or (symbol_to_formula sv1, symbol_to_formula sv2)
      end 
    | ABop (op, sv1, sv2) ->
      begin match op with
      | Lt -> Lt (symbol_to_aterm sv1, symbol_to_aterm sv2)                   
      | Gt -> Lt (symbol_to_aterm sv2, symbol_to_aterm sv1)
      | Le -> Le (symbol_to_aterm sv1, symbol_to_aterm sv2)
      | Ge -> Le (symbol_to_aterm sv2, symbol_to_aterm sv1)
      end
    | EQop (op, sv1, sv2) -> if op = Eq then eq_to_formula op sv1 sv2 else Not (eq_to_formula op sv1 sv2)
    | _ -> raise (Failure "Invalid Formula")

  and eq_to_formula : eq_operator -> symbolic_value -> symbolic_value -> formula
  = fun op sv1 sv2 ->
    match (sv1, sv2) with
    (* Symbol *)
    | Symbol _, _ | _, Symbol _ -> True
    (* Unit *)
    | Unit, Unit -> True
    (* Integer *)
    | _, Int _ | Int _, _ | _, Aop _ | Aop _ , _ | _, Minus _ | Minus _, _ -> Eq (A (symbol_to_aterm sv1),A (symbol_to_aterm sv2))
    (* String *)
    | _, Str _ | Str _, _ | _, Strcon _ | Strcon _ ,_ -> True (* TODO *) 
    (* Bool *)
    | _, Bool _ | Bool _, _ | _, Bop _ | Bop _, _ | _, ABop _ | ABop _, _ | _, EQop _ | EQop _, _ | _, Not _ | Not _, _ -> 
      Iff (symbol_to_formula sv1, symbol_to_formula sv2)
    (* Tuple *)
    | Tuple svs1, Tuple svs2 ->
      (try 
        let fs = List.map2 (fun sv1 sv2 -> eq_to_formula op sv1 sv2) svs1 svs2 in
        connect_formula fs
      with _ -> False)
    (* Constructor *)
    | Ctor (x, svs1), Ctor (y, svs2) ->
      if (x = y) then eq_to_formula op (Tuple svs1) (Tuple svs2) else False
    (* List *)
    | List svs1, List svs2 ->
      (try 
        let fs = List.map2 (fun sv1 sv2 -> eq_to_formula op sv1 sv2) svs1 svs2 in
        connect_formula fs
      with _ -> False)
    | List (sv::svs), Cons (hd, tl) | Cons (hd, tl), List (sv::svs) ->
      let f1 = eq_to_formula op sv hd in
      let f2 = eq_to_formula op (List svs) tl in
      And (f1, f2)
    | Cons (hd1, tl1), Cons (hd2, tl2) -> 
      let f1 = eq_to_formula op hd1 hd2 in
      let f2 = eq_to_formula op tl1 tl2 in
      And (f1, f2)
    (* List Append => Convert to List cons form *)
    | _, Append (l1, l2) | Append (l1, l2), _ -> True (* TODO *)
    | _ -> raise (Failure "Invalid Eq") 

end

module Aez_Encoder = struct
  (* SMT Encoder : fomula -> Aez formula *)

  (* declared symbol set *)
  let symbol_set = ref BatSet.empty

  let rec encode_aterm : aterm -> T.t
  = fun t ->
    match t with
    | ASymbol n -> 
      let s = H.make ("A(" ^ string_of_int n ^ ")") in
      if BatSet.mem s !symbol_set then
        T.make_app s []
      else 
        (Symbol.declare s [] Type.type_int;
        symbol_set := BatSet.add s !symbol_set;
        T.make_app s [])
    | Int n -> T.make_int (Num.Int n)
    | Op (op, t1, t2) ->
      begin match op with
      | Add -> T.make_arith T.Plus (encode_aterm t1) (encode_aterm t2) 
      | Sub -> T.make_arith T.Minus (encode_aterm t1) (encode_aterm t2) 
      | Mul -> T.make_arith T.Mult (encode_aterm t1) (encode_aterm t2) 
      | Div -> T.make_arith T.Div (encode_aterm t1) (encode_aterm t2) 
      | Mod -> T.make_arith T.Modulo (encode_aterm t1) (encode_aterm t2) 
      end

  let rec encode_term : term -> T.t
  = fun t ->
    match t with
    | A t -> encode_aterm t
    (*| S t -> encode_sterm t*)

  let rec encode_formula : formula -> F.t
  = fun f ->
    match f with
    | True -> F.f_true
    | False -> F.f_false
    | FSymbol n -> F.f_true
    | Not f -> F.make F.Not [encode_formula f]
    | And (f1, f2) -> F.make F.And [encode_formula f1; encode_formula f2]
    | Or (f1, f2) -> F.make F.Or [encode_formula f1; encode_formula f2]
    | Iff (f1, f2) -> 
      let (f1, f2) = (encode_formula f1, encode_formula f2) in
      let (imp1, imp2) = (F.make F.Imp [f1; f2], F.make F.Imp [f2; f1]) in
      F.make F.And [imp1; imp2]
    | Lt (t1, t2) -> F.make_lit F.Lt [encode_aterm t1; encode_aterm t2]
    | Le (t1, t2) -> F.make_lit F.Le [encode_aterm t1; encode_aterm t2]
    | Eq (t1, t2) -> F.make_lit F.Eq [encode_term t1; encode_term t2]
end

(********************* SMT Pruning ***********************) 

let cache = ref Cache.empty

let try_count = ref 0
let new_try () = (try_count := !try_count + 1); !try_count

let unsat_count = ref 0
let smt_time = ref 0.0
(*
let out1 = ref (open_out "sat.txt")
let out2 = ref (open_out "unsat.txt")
*)
let solve : symbolic_value -> bool
= fun sv ->
  let f =
    try 
      Converter.symbol_to_formula sv 
    with _ -> False (* Invalid Formula e.g) type miss match *)
  in
  let aez_formula = Aez_Encoder.encode_formula f in
  try
    Solver.clear ();
    Solver.assume ~profiling:false ~id:(new_try ()) aez_formula;
    Solver.check ~profiling:false ();
    true (*SAT*)
  with 
    | Unsat _ -> (* UNSAT *) false
    | _ -> true (* Assertion Fail *)

let smt_pruning : prog -> examples -> bool
= fun pgm examples ->
  let start_time = Unix.gettimeofday() in
  let pgm' = translate pgm in
  let key = Print.program_to_string pgm in
  let result = 
    if Cache.mem key !cache then
      Cache.find key !cache
    else
      let svs = List.map (Symbol_eval.gen_constraint pgm') examples in
      let t = List.for_all solve svs in
      cache := Cache.add key t !cache;
      t
  in
  let _ =
    (* checking *)
    unsat_count := if result then !unsat_count else 1 + !unsat_count;
    smt_time := (!smt_time)+.(Unix.gettimeofday() -. start_time);
  in
  (* Debuging *)
  (*let svs = List.map (Symbol_eval.gen_constraint pgm') examples in
  let s = List.fold_left (fun acc sv -> acc ^ "\n" ^ Print.symbol_to_string sv) "" svs in 
  let str = Print.program_to_string pgm ^ s in
  let _ =
    if result then
      Printf.fprintf (!out1) "--------------------------------\n%s\n" (str)
    else
      Printf.fprintf (!out2) "--------------------------------\n%s\n" (str)
  in*)
  result
