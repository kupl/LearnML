open Lang
open Util
open Symbol_lang2

type eq_class = symbolic_value BatSet.t
(* Normalize the result of symbolic execution *)
(* Rule based approaches *)
(* Negation *)
let inverse_op : operator -> operator
= fun op ->
  match op with
  | Add -> Sub
  | Sub -> Add
  | Div -> Mul
  | Mul -> Div
  | Mod -> raise (Failure "Invalid Inverse")

let negate_eq : eq_operator -> eq_operator
= fun op -> 
  match op with
  | Eq -> NEq
  | NEq -> Eq

let negate_comb : combinator -> combinator
= fun op ->
  match op with
  | And -> Or
  | Or -> And

let negate_comp : comparator -> comparator
= fun op ->
  match op with
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

(* reordering *)
let rec reorder_sym_val : symbolic_value -> symbolic_value
= fun sv ->
  let rec arg_num : symbolic_value -> int
  = fun sv ->
    match sv with      
    | List svs | Tuple svs | Ctor (_, svs) -> List.fold_left (fun acc sv -> acc + arg_num sv) 0 svs
    | Minus sv | Not sv -> arg_num sv
    | Aop (_, sv1, sv2) | Bop (_, sv1, sv2) | ABop (_, sv1, sv2) | EQop (_, sv1, sv2)
    | Cons (sv1, sv2) | Append (sv1, sv2) | Strcon (sv1, sv2) -> (arg_num sv1) + (arg_num sv2)
    | _ -> 1
  in
  let compare : symbolic_value -> symbolic_value -> bool
  = fun sv1 sv2 ->
    match sv1, sv2 with
    | ASymbol n1, ASymbol n2 | SSymbol n1, SSymbol n2 -> n1 > n2
    | ASymbol _, SSymbol _ -> true 
    | SSymbol _, ASymbol _ -> false
    | ASymbol _, _ | SSymbol _, _ -> true
    | _, ASymbol _ | _, SSymbol _ -> false
    | _ -> arg_num sv1 > arg_num sv2
  in
  match sv with
  | List svs -> List (List.map reorder_sym_val svs)
  | Tuple svs -> Tuple (List.map reorder_sym_val svs)
  | Ctor (x, svs) -> Ctor (x, List.map reorder_sym_val svs)
  | Minus sv -> Minus (reorder_sym_val sv)
  | Not sv -> Not (reorder_sym_val sv)
  (* binary operation *)
  | Aop (op, sv1, sv2) ->
    begin match op with
    | Add | Mul -> if (compare sv1 sv2) then Aop (op, reorder_sym_val sv2, reorder_sym_val sv1) else Aop (op, reorder_sym_val sv1, reorder_sym_val sv2)
    | _ -> Aop (op, reorder_sym_val sv1, reorder_sym_val sv2)
    end
  | Bop (op, sv1, sv2) -> if (compare sv1 sv2) then Bop (op, reorder_sym_val sv2, reorder_sym_val sv1) else Bop (op, reorder_sym_val sv1, reorder_sym_val sv2)
  | ABop (op, sv1, sv2) -> ABop (op, reorder_sym_val sv1, reorder_sym_val sv2)
  | EQop (op, sv1, sv2) -> if (compare sv1 sv2) then EQop (op, reorder_sym_val sv2, reorder_sym_val sv1) else EQop (op, reorder_sym_val sv1, reorder_sym_val sv2)
  | Cons (sv1, sv2) -> Cons (reorder_sym_val sv1, reorder_sym_val sv2)
  | Append (sv1, sv2) -> Append (reorder_sym_val sv1, reorder_sym_val sv2)
  | Strcon (sv1, sv2) -> Strcon (reorder_sym_val sv1, reorder_sym_val sv2)
  | _ -> sv 

let rec normalize_sym_val : symbolic_value -> symbolic_value
= fun sv ->
  match sv with
  | List svs -> List (List.map normalize_sym_val svs)
  | Tuple svs -> Tuple (List.map normalize_sym_val svs)
  | Ctor (x, svs) -> Ctor (x, List.map normalize_sym_val svs)
  (* unary operation *)
  | Minus sv ->
    begin match normalize_sym_val sv with
    | Int n -> Int (-n)
    | Aop (op, sv1, sv2) -> normalize_sym_val (Aop (op, Aop (Mul, Int (-1), sv1), Aop (Mul, Int (-1), sv2)))
    | Minus sv -> sv
    | sv -> Minus sv
    end
  | Not sv ->
    begin match normalize_sym_val sv with
    | Bool b -> Bool (not b)
    | Bop (op, sv1, sv2) -> normalize_sym_val (Bop (negate_comb op, Not sv1, Not sv2))
    | ABop (op, sv1, sv2) -> normalize_sym_val (ABop (negate_comp op, sv1, sv2))
    | EQop (op, sv1, sv2) -> normalize_sym_val (EQop (negate_eq op, sv1, sv2))
    | sv -> Not sv
    end
  (* binary operation *)
  | Aop (op, sv1, sv2) -> normalize_aop op (normalize_sym_val sv1) (normalize_sym_val sv2)
  | Bop (op, sv1, sv2) -> normalize_bop op (normalize_sym_val sv1) (normalize_sym_val sv2)
  | ABop (op, sv1, sv2) -> normalize_abop op (normalize_sym_val sv1) (normalize_sym_val sv2)
  | EQop (op, sv1, sv2) -> normalize_eqop op (normalize_sym_val sv1) (normalize_sym_val sv2)
  | Cons (sv1, sv2) -> 
    begin match (normalize_sym_val sv1, normalize_sym_val sv2) with
    | sv1, List svs -> List (sv1::svs)
    | sv1, sv2 -> Cons (sv1, sv2)
    end
  | Strcon (sv1, sv2) -> 
    begin match (normalize_sym_val sv1, normalize_sym_val sv2) with
    | Str sv1, Str sv2 -> Str (sv1 ^ sv2)
    | sv1, sv2 -> Strcon (sv1, sv2)
    end
  | Append (sv1, sv2) -> 
    begin match (normalize_sym_val sv1, normalize_sym_val sv2) with
    | List [], sv | sv, List [] -> sv 
    | List svs1, List svs2 -> List (svs1@svs2)
    | sv1, sv2 -> Append (sv1, sv2)
    end
  | _ -> sv

and normalize_aop : operator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  match op with
  | Add ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 + n2)
    | Int 0, sv | sv, Int 0 -> sv
    | sv1, Minus sv2 -> normalize_sym_val (Aop (Sub, sv1, sv2))
    | Int n1, Aop (Add, Int n2, sv') | Int n1, Aop (Add, sv', Int n2) -> normalize_sym_val (Aop (Add, Int (n1 + n2), sv'))
    | Int n1, Aop (Sub, Int n2, sv') -> normalize_sym_val (Aop (Sub, Int (n1 + n2), sv'))
    | Aop (Add, Int n1, sv'), Int n2 | Aop (Add, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Add, Int (n1 + n2), sv'))
    | Aop (Sub, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Add, Int (n2 - n1), sv'))
    | _ -> Aop (op, sv1, sv2)
    end
  | Sub -> 
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 - n2)
    | Int 0, sv -> normalize_sym_val (Minus sv)
    | sv, Int 0 -> sv
    | sv, Int n when n < 0 -> Aop (Add, sv, Int (-n))
    | sv1, Aop (Add, sv1', sv1'') when (sv1 = sv1') -> normalize_sym_val (Minus sv1'')
    | sv1, Aop (Add, sv1', sv1'') when (sv1 = sv1'') -> normalize_sym_val (Minus sv1'')
    | sv1, Aop (Sub, sv1', sv1'') when (sv1 = sv1') -> normalize_sym_val sv1
    | sv1, Aop (Sub, sv1', sv1'') when (sv1 = sv1'') -> normalize_sym_val (Minus sv1')
    | Int n1, Aop (Sub, Int n2, sv') -> normalize_sym_val (Aop (Add, Int (n1 - n2), sv'))
    | Int n1, Aop (Sub, sv', Int n2) -> normalize_sym_val (Aop (Sub, Int (n1 + n2), sv'))
    | Aop (Sub, Int n1, sv'), Int n2 -> normalize_sym_val (Aop (Sub, Int (n1 - n2), sv'))
    | Aop (Sub, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Sub, sv', Int (n1 + n2)))
    | Aop (Add, sv', Int n1), Int n2 | Aop (Add, Int n1, sv'), Int n2 -> normalize_sym_val (Aop (Add, sv', Int (n1 - n2)))
    | sv1, Minus sv2 -> normalize_sym_val (Aop (Add, sv1, sv2)) 
    | sv1, sv2 -> if sv1 = sv2 then Int 0 else Aop (op, sv1, sv2)
    end
  | Mul ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 * n2)
    | Int 0, _ | _, Int 0 -> Int 0
    | Int 1, sv | sv, Int 1 -> sv
    | Int (-1), sv | sv, Int (-1) -> normalize_sym_val (Minus sv)
    | Int n1, Aop (Mul, Int n2, sv') | Int n1, Aop (Mul, sv', Int n2) -> normalize_sym_val (Aop (Mul, Int (n1 * n2), sv'))
    | Aop (Mul, Int n1, sv'), Int n2 | Aop (Mul, sv', Int n1), Int n2  -> normalize_sym_val (Aop (Mul, Int (n1 * n2), sv'))
    | _ -> Aop (op, sv1, sv2)
    end
  | Div ->
    begin match (sv1, sv2) with
    | _, Int 0 -> Exn 
    | Int n1, Int n2 -> Int (n1 / n2)
    | Int 0, _ -> Int 0
    | _, Int 1 -> sv1
    | Aop (Div, sv', Int n1), Int n2  -> normalize_sym_val (Aop (Div, sv' , Int (n1 * n2)))
    | _ -> Aop (op, sv1, sv2)
    end
  | Mod ->
    begin match (sv1, sv2) with
    | _, Int 0 -> Exn
    | Int n1, Int n2 -> Int (n1 mod n2)
    | Int 0, _ | _, Int 1 -> Int 0
    | _ -> Aop (op, sv1, sv2)
    end

and normalize_bop : combinator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  match op with
  | And ->
    begin match (sv1, sv2) with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | Bool true, sv | sv, Bool true -> sv
    | Bool false, _ | _, Bool false -> Bool false
    | _ -> Bop (op, sv1, sv2)
    end
  | Or ->
    begin match (sv1, sv2) with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | Bool false, sv | sv, Bool false -> sv
    | Bool true, _ | _, Bool true -> Bool true
    | _ -> Bop (op, sv1, sv2)
    end

and normalize_abop : comparator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->
  let min_int = -4611686018427387903 in
  let max_int = 4611686018427387903 in
  match (sv1, sv2) with
  | Minus sv, Int n -> ABop (op, normalize_sym_val (Minus sv2), sv)
  | Int n, Minus sv -> ABop (op, sv, normalize_sym_val (Minus sv1))
  | Int n1, Int n2 ->
    begin match op with
    | Lt -> Bool (n1 < n2)
    | Gt -> Bool (n1 > n2)
    | Le -> Bool (n1 <= n2)
    | Ge -> Bool (n1 >= n2)
    end
  (* Boundary check *)
  | Int n1, _ ->
    begin match op with
    | Lt -> if n1 = max_int then Bool false else if n1 = min_int then Bool true else ABop (op, sv1, sv2)
    | Gt -> if n1 = min_int then Bool false else if n1 = max_int then Bool true else ABop (op, sv1, sv2)
    | Le -> if n1 = max_int then Bool false else if n1 = min_int then Bool true else ABop (op, sv1, sv2)
    | Ge -> if n1 = min_int then Bool false else if n1 = max_int then Bool true else ABop (op, sv1, sv2)
    end  
  | _, Int n2 ->
    begin match op with
    | Lt -> if n2 = min_int then Bool false else if n2 = max_int then Bool true else ABop (op, sv1, sv2)
    | Gt -> if n2 = max_int then Bool false else if n2 = min_int then Bool true else ABop (op, sv1, sv2)
    | Le -> if n2 = min_int then Bool false else if n2 = max_int then Bool true else ABop (op, sv1, sv2)
    | Ge -> if n2 = max_int then Bool false else if n2 = min_int then Bool true else ABop (op, sv1, sv2)
    end
  | _ -> 
    begin match op with
    | Lt | Gt -> if sv1 = sv2 then Bool false else ABop (op, sv1, sv2)
    | _ -> ABop (op, sv1, sv2)
    end  

and normalize_eqop : eq_operator -> symbolic_value -> symbolic_value -> symbolic_value
= fun op sv1 sv2 ->  
  let sv' = 
    match op with 
    | Eq -> 
      begin match (sv1, sv2) with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | Str s1, Str s2 -> Bool (s1 = s2)
      | List svs1, List svs2 | Tuple svs1, Tuple svs2 -> (
        try 
          let sv = List.fold_left2 (fun sv sv1 sv2 -> Bop (And, sv, EQop (Eq, sv1, sv2))) (Bool true) svs1 svs2 in
          normalize_sym_val sv
        with _ -> Bool false)
      | Ctor (x, svs1), Ctor (y, svs2) -> 
        if x = y then  
          let sv = List.fold_left2 (fun sv sv1 sv2 -> Bop (And, sv, EQop (Eq, sv1, sv2))) (Bool true) svs1 svs2 in
          normalize_sym_val sv
        else Bool false
      | sv1, sv2 -> if sv1 = sv2 then Bool true else EQop (op, sv1, sv2)
      end
    | NEq ->
      begin match (sv1, sv2) with
      | Int n1, Int n2 -> Bool (n1 <> n2)
      | Bool b1, Bool b2 -> Bool (b1 <> b2)
      | Str s1, Str s2 -> Bool (s1 <> s2)
      | List svs1, List svs2 | Tuple svs1, Tuple svs2 -> (
        try 
          let sv = List.fold_left2 (fun sv sv1 sv2 -> Bop (Or, sv, EQop (NEq, sv1, sv2))) (Bool false) svs1 svs2 in
          normalize_sym_val sv
        with _ -> Bool false)
      | Ctor (x, svs1), Ctor (y, svs2) -> 
        if x = y then  
          let sv = List.fold_left2 (fun sv sv1 sv2 -> Bop (Or, sv, EQop (NEq, sv1, sv2))) (Bool false) svs1 svs2 in
          normalize_sym_val sv
        else Bool true
      | sv1, sv2 -> if sv1 = sv2 then Bool false else EQop (op, sv1, sv2)
      end
  in
  reorder_sym_val sv'

let rec has_eq : path_cond -> symbolic_value -> symbolic_value -> bool
= fun pc sv1 sv2 -> (BatSet.mem (EQop (Eq, sv1, sv2)) pc) || (BatSet.mem (EQop (Eq, sv2, sv1)) pc)

let rec has_lt : path_cond -> symbolic_value -> symbolic_value -> bool
= fun pc sv1 sv2 -> BatSet.mem (ABop (Lt, sv1, sv2)) pc

let rec has_gt : path_cond -> symbolic_value -> symbolic_value -> bool
= fun pc sv1 sv2 -> BatSet.mem (ABop (Gt, sv1, sv2)) pc

(* TODO *)
let rec normalize_pc : path_cond -> path_cond -> path_cond
= fun pc pc' -> 
  if BatSet.is_empty pc then pc'
  else 
    let (sv, pc) = BatSet.pop pc in
    let sv' = normalize_sym_val sv in
    let pc' = 
      match sv' with
      | Bool true -> pc'
      | Bool false -> gen_pc (Bool false)
      | Bop (And, sv1, sv2) -> BatSet.add sv2 (BatSet.add sv1 pc')
      | ABop (comp, sv1, sv2) ->
        begin match sv1, sv2 with
        | Aop (Add, Int n1, sv1), Aop (op, Int n2, sv2) | Aop (Add, sv1, Int n1), Aop (op, Int n2, sv2) -> 
          let sv' = normalize_sym_val (ABop (comp, sv1, Aop (op, Int (n2 - n1), sv2))) in
          BatSet.add sv' pc'
        | Aop (Add, Int n1, sv1), Aop (op, sv2, Int n2) | Aop (Add, sv1, Int n1), Aop (op, sv2, Int n2) -> 
          let sv' = normalize_sym_val (ABop (comp, sv1, Aop (op, sv2, Int (n2 - n1)))) in
          BatSet.add sv' pc'
        | _ -> BatSet.add sv' pc'
        end
      | _ -> BatSet.add sv' pc'
    in
    normalize_pc pc pc'

(*********************)
(* remove unsat path *)
(*********************)

(* Check given sv is integer *)
let is_pos : symbolic_value -> bool
= function
  | Int n -> n >= 0
  | _ -> false

let is_neg : symbolic_value -> bool
= function 
  | Int n -> n < 0
  | _ -> false

let is_int : symbolic_value -> bool
= function
  | Int _ -> true
  | _ -> false

(* Check sv1 is arithmetic operation with sv2 and other constant *)
let is_add : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Add, sv1', sv2') -> (sv1' = sv2 && is_int sv2') || (sv2' = sv2 && is_int sv1')
  | _ -> false  

let is_add2 : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Add, sv1', sv2') -> (sv1' = sv2 && is_pos sv2') || (sv2' = sv2 && is_pos sv1')
  | _ -> false  

let is_sub : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Sub, sv1', sv2') -> (sv1' = sv2 && is_int sv2')
  | _ -> false  

let is_sub2 : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Sub, sv1', sv2') -> (sv1' = sv2 && is_pos sv2')
  | _ -> false 

let is_mul : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Mul, sv1', sv2') -> (sv1' = sv2 && is_int sv2') || (sv2' = sv2 && is_int sv1')
  | _ -> false  

let is_div : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Div, sv1', sv2') -> (sv1' = sv2 && is_int sv2')
  | _ -> false  

let is_aop : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 -> (is_add sv1 sv2) || (is_sub sv1 sv2) || (is_mul sv1 sv2) || (is_div sv1 sv2)

(* Heuristics *)
(* 
  1. sv1 = n1 && sv1 = n2 && n1 <> n2
  2. An eq_relation sv is unsat if there exist sv1 = sv2 and sv is sv2 = (sv1 aop const) 
*)
(* Need Refactoring this parts with above predicates *)
let rec is_unsat_eq : symbolic_value -> symbolic_value -> symbolic_value -> bool
= fun sv sv1 sv2 ->
  match sv with
  | EQop (Eq, Aop (op, sv1', sv2'), sv) | EQop (Eq, sv, Aop (op, sv1', sv2')) ->
    if op <> Mod then
      if sv = sv1 then 
        if sv1' = sv2 && is_int sv2' then true
        else if sv2' = sv2 && is_int sv1' then true
        else false
      else if sv = sv2 then
        if sv1' = sv1 && is_int sv2' then true
        else if sv2' = sv1 && is_int sv1' then true
        else false
      else false  
    else false
  | _ -> false
(* An eq_relation sv is unsat if there exist sv1 <= sv2 and sv is sv2 = (sv1 - pos) *)
(*
1. (#A (3) <= #A (1)) /\ (#A (3) > (1 + #A (1)))
2. (#A (1) <= (#A (2) - 1)) /\ (#A (1) > #A (2))
3. ((2 + #A (1)) = #A (2)) /\ (((3 + #A (1)) <= #A (2))
*)

let comp_sat_checker : comparator -> path_cond -> symbolic_value -> symbolic_value -> bool
= fun op pc sv1 sv2 ->
  match op with
  | Lt | Le -> 
    BatSet.exists (fun sv -> 
      match sv with
      | EQop (Eq, sv1', sv2') -> 
        if sv1' = sv2 && is_sub2 sv2' sv1 then true
        else if sv2' = sv2 && is_sub2 sv1' sv1 then true
        else false
      | ABop (Gt, sv1', sv2') | ABop (Ge, sv1', sv2') -> 
        if sv1' = sv1 && is_add2 sv2' sv2 then true
        else false
      | _ -> false 
    ) pc
  | Gt | Ge -> 
    BatSet.exists (fun sv -> 
      match sv with
      | EQop (Eq, sv1', sv2') -> 
        if sv1' = sv1 && is_sub2 sv2' sv2 then true
        else if sv2' = sv1 && is_sub2 sv1' sv2 then true
        else false
      | ABop (Le, sv1', sv2') | ABop (Lt, sv1', sv2') ->
        if sv1' = sv1 && is_sub2 sv2' sv2 then true
        else false
      | _ -> false 
    ) pc

(* 
  If sv1 + pos <= sv1 then unsat 
 *)
let comp_sat_checker2 : comparator -> symbolic_value -> symbolic_value -> bool
= fun op sv1 sv2 ->
  match op with
  | Lt -> 
    begin match sv1, sv2 with
    | Aop (Add, Int n, sv1), sv2 | Aop (Add, sv1, Int n), sv2 -> sv1 = sv2 && n >= 0 
    | sv1, Aop (Add, Int n, sv2) | sv1, Aop (Add, sv2, Int n) -> sv1 = sv2 && n <= 0
    | sv1, Aop (Sub, sv2, Int n) -> sv1 = sv2 && n >= 0  
    | _ -> false
    end
  | Le ->
    begin match sv1, sv2 with
    | Aop (Add, Int n, sv1), sv2 | Aop (Add, sv1, Int n), sv2 -> sv1 = sv2 && n > 0 
    | sv1, Aop (Add, Int n, sv2) | sv1, Aop (Add, sv2, Int n) -> sv1 = sv2 && n < 0
    | sv1, Aop (Sub, sv2, Int n) -> sv1 = sv2 && n > 0 
    | _ -> false
    end
  | Gt ->
    begin match sv1, sv2 with
    | Aop (Sub, sv1, Int n), sv2 -> sv1 = sv2 && n >= 0 
    | sv1, Aop (Add, sv2, Int n) | sv1, Aop (Add, Int n, sv2) -> sv1 = sv2 && n >= 0 
    | _ -> false
    end
  | Ge ->
    begin match sv1, sv2 with
    | Aop (Sub, sv1, Int n), sv2 -> sv1 = sv2 && n > 0 
    | sv1, Aop (Add, sv2, Int n) | sv1, Aop (Add, Int n, sv2) -> sv1 = sv2 && n > 0 
    | _ -> false
    end

let rec is_sat_pc : path_cond -> path_cond -> bool
= fun pc pc' ->
  if BatSet.is_empty pc then true
  else
    let (sv, pc) = BatSet.pop pc in
    if BatSet.mem (Not sv) pc' then false else
    match sv with
    | Bop (Or, sv1, sv2) -> if not (is_sat_pc (BatSet.singleton sv1) pc') && not (is_sat_pc (BatSet.singleton sv2) pc') then false else is_sat_pc pc pc'
    | Bool false -> false
    | EQop (op, sv1, sv2) -> 
      if (BatSet.mem (EQop (negate_eq op, sv1, sv2)) pc' || BatSet.mem (EQop (negate_eq op, sv2, sv1)) pc') then false 
      else
        begin match op with
        | Eq -> if BatSet.exists (fun sv -> is_unsat_eq sv sv1 sv2) pc' then false else is_sat_pc pc pc'
        | NEq -> is_sat_pc pc pc' 
        end
    | ABop (op, sv1, sv2) -> 
      if BatSet.mem (ABop (negate_comp op, sv1, sv2)) pc' then false
      else if comp_sat_checker op pc' sv1 sv2 then false
      else if comp_sat_checker2 op sv1 sv2 then false
      else
        begin match op with
        | Lt | Gt -> if (BatSet.mem (ABop (op, sv2, sv1)) pc') || (BatSet.mem (EQop (Eq, sv2, sv1)) pc') || (BatSet.mem (EQop (Eq, sv1, sv2)) pc') then false else is_sat_pc pc pc'
        | _ -> is_sat_pc pc pc'
        end
    | _ -> is_sat_pc pc pc' 

(* Main procedures while evaluation *)
let normalize_formula : sym_formula -> sym_formula
= fun psi ->
  psi 
  |> BatSet.map (fun (pc, sv) -> (normalize_pc pc BatSet.empty, normalize_sym_val sv))

let filter_formula : sym_formula -> sym_formula
= fun psi ->
  psi 
  |> BatSet.filter (fun (pc, sv) -> is_sat_pc pc pc)
  |> BatSet.filter (fun (pc, sv) -> (not (is_exn sv) && BatSet.for_all (fun sv -> not (is_exn sv)) pc)) (* Remove exceptional value *)

let rec run : sym_formula -> sym_formula
= fun psi ->
  let one_step x = 
    let x = normalize_formula x in
    let x = filter_formula x in
    x
  in
  one_step psi

(* Main procedures after evaluation *)
(* Remove redundant comp condtion in pc after evaluation *)
let rec normalize_pc2 : path_cond -> path_cond -> path_cond
= fun pc pc' ->
  if BatSet.is_empty pc then pc'
  else 
    let (sv', pc) = BatSet.pop pc in
    let pc' = 
      (* Formula Agumentation1 *)
      match sv' with
      | EQop (eq, sv1, sv2) ->
        let sv' =
          begin match sv1 with
          | Aop (op, sv, Int n) -> EQop (eq, sv, normalize_sym_val (Aop (inverse_op op, sv2, Int n)))
          | _ -> sv'
          end
        in
        let sv' = 
          begin match sv2 with
          | Aop (op, sv, Int n) -> EQop (eq, normalize_sym_val (Aop (inverse_op op, sv1, Int n)), sv)
          | _ -> sv'
          end
        in
        BatSet.add sv' pc'
      | ABop (comp, sv1, sv2) ->
        let sv' = 
          begin match sv1 with
          | Aop (op, sv, Int n) -> ABop (comp, sv, normalize_sym_val (Aop (inverse_op op, sv2, Int n)))
          | _ -> sv'
          end
        in
        let sv' = 
          begin match sv2 with
          | Aop (op, sv, Int n) -> ABop (comp, normalize_sym_val (Aop (inverse_op op, sv1, Int n)), sv)
          | _ -> sv'
          end
        in
        let sv' =
          begin match sv1, sv2 with
          | Int 0, Aop (Sub, sv1', sv2') -> ABop (comp, sv2', sv1')
          | Aop (Sub, sv1', sv2'), Int 0 -> ABop (comp, sv1', sv2')
          | Int 0, Aop (Add, Int n, sv2') -> ABop (comp, Int (-n), sv2')
          | Aop (Add, sv1', Int n), Int 0 -> ABop (comp, sv1', Int (-n))
          | _ -> sv'
          end
        in
        BatSet.add sv' pc'
      | _ -> pc'
    in
    normalize_pc2 pc pc'

let rec agument_pc : path_cond -> path_cond -> path_cond
= fun pc pc' ->
  if BatSet.is_empty pc then pc'
  else 
    let (sv', pc) = BatSet.pop pc in
    let pc' = 
      (* Formula Agumentation2 *)
      match sv' with
      | ABop (Le, sv1, Int n2) ->
        if BatSet.exists (fun sv' ->
          match sv' with
          | ABop (Gt, sv1, Int n2') when n2' = n2 - 1 -> true
          | ABop (Ge, sv1, Int n2') when n2' = n2 -> true
          | ABop (Le, Int n2', sv1) when n2' = n2 -> true
          | ABop (Lt, Int n2', sv1) when n2' = n2 - 1 -> true
          | _ -> false
        ) pc' then BatSet.add (EQop (Eq, sv1, Int n2)) pc' else pc'
      | ABop (Ge, Int n1, sv2) ->
        if BatSet.exists (fun sv' ->
          match sv' with
          | ABop (Gt, sv2, Int n1') when n1' = n1 - 1 -> true
          | ABop (Ge, sv2, Int n1') when n1' = n1 -> true
          | ABop (Le, Int n1', sv2) when n1' = n1 -> true
          | ABop (Lt, Int n1', sv2) when n1' = n1 - 1 -> true
          | _ -> false
        ) pc' then BatSet.add (EQop (Eq, Int n1, sv2)) pc' else pc'
      | _ -> pc'
    in
    agument_pc pc pc'

let rec is_sat_pc2 : path_cond -> path_cond -> bool
= fun pc pc' ->
  if BatSet.is_empty pc then true
  else
    let (sv, pc) = BatSet.pop pc in
    match sv with
    | Bop (Or, sv1, sv2) -> if not (is_sat_pc2 (BatSet.singleton sv1) pc') && not (is_sat_pc2 (BatSet.singleton sv2) pc') then false else is_sat_pc2 pc pc'
    | EQop (Eq, sv1, sv2) ->
      begin match sv1, sv2 with
      | sv1, Int n1 | Int n1, sv1 -> if (BatSet.exists (fun sv' -> 
                match sv' with
                | EQop (Eq, sv2, Int n2) | EQop (Eq, Int n2, sv2) -> 
                  if sv1 = sv2 then n1 <> n2
                  (*
                    ((2 = #A (1)) /\ (4 = #A (2)) 
                    (2 + #A (1)) <> #A (2)
                  *)
                  else 
                    if (BatSet.exists (fun sv'' ->
                      match sv'' with
                      | EQop (eq, Aop (op, sv1', sv2'), sv3') | EQop (eq, sv3',  Aop (op, sv1', sv2')) ->
                        if sv1 = sv1' && sv2 = sv3' then (Bool false) = normalize_sym_val (EQop (eq, Aop (op, Int n1, sv2'), Int n2))
                        else if sv1 = sv2' && sv2 = sv3' then (Bool false) = normalize_sym_val (EQop (eq, Aop (op, sv1', Int n1), Int n2))
                        else if sv2 = sv1' && sv1 = sv3' then (Bool false) = normalize_sym_val (EQop (eq, Aop (op, Int n2, sv2'), Int n1))
                        else if sv2 = sv2' && sv1 = sv3' then (Bool false) = normalize_sym_val (EQop (eq, Aop (op, sv1', Int n2), Int n1))
                        else false
                      | ABop (comp, Aop (op, sv1', sv2'), sv3') | ABop (comp, sv3', Aop (op, sv1', sv2')) -> false (* TODO *)
                      | _ -> false
                    ) pc') then true else false
                | ABop (comp, sv2, Int n2) when (sv1 = sv2 && n1 > n2) -> (comp = Lt) || (comp = Le)
                | ABop (comp, Int n2, sv2) when (sv1 = sv2 && n1 < n2) -> (comp = Gt) || (comp = Ge)
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      
          (*
          BatSet.exists (fun sv3' ->
                    match sv3' with
                    | EQop (eq, Aop (op, sv1'', sv''), sv2'') | EQop (eq, Aop (op, sv'', sv1''), sv2'')
                    | EQop (eq, sv2'', Aop (op, sv1'', sv'')) | EQop (eq, sv2'', Aop (op, sv'', sv1'')) ->
                      let _ = print_endline ("000000") in
                      let _ = print_endline ("Sv' : " ^ symbol_to_string sv') in
                      if (sv1' = sv2'') && (sv1 = sv1'') then 
                        let _ = print_endline ("B1 : " ^ symbol_to_string (EQop (eq, Aop (op, Int n2, sv''), Int n2'))) in
                        (Bool false) = normalize_sym_val (EQop (eq, Aop (op, Int n2, sv''), Int n2')) 
                      else if (sv1 = sv2'') && (sv1' = sv1'') then 
                        let _ = print_endline ("B2 : " ^ symbol_to_string (EQop (eq, Aop (op, Int n2', sv''), Int n2))) in
                        (Bool false) = normalize_sym_val (EQop (eq, Aop (op, Int n2', sv''), Int n2)) 
                      else false
                    | _ -> false
                  ) pc' 
          *)
      | _ -> is_sat_pc2 pc pc'
      end      
    | ABop (Gt, sv1, sv2) -> 
      begin match sv1, sv2 with
      | sv1, Int n2 -> if (BatSet.exists (fun sv' -> 
                match sv' with
                | ABop (comp, sv1', Int n2') when (sv1 = sv1' && n2 >= n2') -> (comp = Lt) || (comp = Le)
                | ABop (comp, Int n2', sv1') when (sv1 = sv1' && n2 >= n2') -> (comp = Gt) || (comp = Ge)
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      | _ -> is_sat_pc2 pc pc'
      end
    | ABop (Ge, sv1, sv2) -> 
      begin match sv1, sv2 with
      | sv1, Int n2 -> if (BatSet.exists (fun sv' -> 
                match sv' with
                | ABop (comp, sv1', Int n2') when (sv1 = sv1' && n2 > n2') -> (comp = Lt) || (comp = Le)
                | ABop (comp, Int n2', sv1') when (sv1 = sv1' && n2 > n2') -> (comp = Gt) || (comp = Ge)
                | ABop (comp, Aop (Add, Int n1', sv1'), Int n2') | ABop (comp, Aop (Add, sv1', Int n1'), Int n2') ->
                  if n2 = n2' && n1' > 0 && sv1 = sv1' then (comp = Lt) || (comp = Le) else false
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      | _ -> is_sat_pc2 pc pc'
      end
    | ABop (Le, sv1, sv2) ->
      begin match sv1, sv2 with
      | sv1, Int n2 -> if (BatSet.exists (fun sv' ->
                match sv' with
                | ABop (comp, sv1', Int n2') when (sv1 = sv1' && n2' >= n2) -> (comp = Gt) || (comp = Ge)
                | ABop (comp, Int n2', sv1') when (sv1 = sv1' && n2' >= n2) -> (comp = Lt) || (comp = Le)
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      (* Find grain case :      
        #A (1) <= #A (2)  
        (2 = #A (1)
        (#A (2) <= 1)
      *)
      | ASymbol a1, ASymbol a2 -> if (BatSet.exists (fun sv' ->
                match sv' with
                | EQop (Eq, sv1', Int n1') | EQop (Eq, Int n1', sv1') ->
                  if sv1' = sv1 then BatSet.exists (fun sv' ->
                    match sv' with
                    | ABop (comp, sv2', Int n2') when sv2 = sv2' && n2' < n1' -> (comp = Lt) || (comp = Le)
                    | _ -> false
                  ) pc' else false
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      | _ -> is_sat_pc2 pc pc'
      end
    | ABop (Lt, sv1, sv2) ->
      begin match sv1, sv2 with
      | sv1, Int n2 -> if (BatSet.exists (fun sv' ->
                match sv' with
                | ABop (comp, sv1', Int n2') when (sv1 = sv1' && n2' > n2) -> (comp = Gt) || (comp = Ge)
                | ABop (comp, Minus sv1', Int n2') when (sv1 = sv1' && (n2' <= n2 || n2' = n2 + 1)) -> (comp = Lt) || (comp = Le)
                | ABop (comp, Int n2', sv1') when (sv1 = sv1' && n2' > n2) -> (comp = Lt) || (comp = Le)
                | _ -> false
              ) pc') then false else is_sat_pc2 pc pc'
      | _ -> is_sat_pc2 pc pc'
      end
    | _ -> is_sat_pc2 pc pc' 

let get_neq_relations : path_cond -> (symbolic_value * symbolic_value) BatSet.t 
= fun pc ->
  BatSet.fold (fun sv acc ->
    match sv with
    | EQop (NEq, sv1, sv2) -> BatSet.add (sv1, sv2) acc
    | _ -> acc
  ) pc BatSet.empty

let get_eq_classes : path_cond -> eq_class BatSet.t
= fun pc ->
  let eq_rels = BatSet.fold (fun sv acc ->
    match sv with
    | EQop (Eq, sv1, sv2) -> BatSet.add (BatSet.add sv2 (BatSet.singleton sv1)) acc
    | _ -> acc
  ) pc BatSet.empty
  in
  let rec iter : eq_class BatSet.t -> eq_class BatSet.t -> eq_class BatSet.t
  = fun t t' ->
    let t' = BatSet.fold (fun eq_class t' ->
      if BatSet.for_all (fun eq_class' -> BatSet.disjoint eq_class eq_class') t' then BatSet.add eq_class t' 
      else 
        BatSet.map (fun eq_class' ->
          if not (BatSet.disjoint eq_class eq_class') then BatSet.union eq_class eq_class' else eq_class'
        ) t'
    ) t t'
    in
    if BatSet.equal t t' then t' else iter t' BatSet.empty
  in
  iter eq_rels BatSet.empty

let rec filter_unsat_class : path_cond -> bool
= fun pc ->
  let neq_relations = get_neq_relations pc in
  let eq_classes = get_eq_classes pc in
  not (BatSet.exists (fun (sv1, sv2) -> BatSet.exists (fun eq_class -> BatSet.mem sv1 eq_class && BatSet.mem sv2 eq_class) eq_classes) neq_relations)

let rec update_eq_class : path_cond -> path_cond
= fun pc ->
  let eq_classes = get_eq_classes pc in
  let rec gen_pairs : eq_class -> (symbolic_value * symbolic_value) BatSet.t
  = fun eq ->
    if BatSet.is_empty eq then BatSet.empty
    else
      let (sv1, eq') = BatSet.pop eq in
      BatSet.union (gen_pairs eq') (BatSet.map (fun sv2 -> (sv1, sv2)) eq')
  in
  let eq_pairs_set = BatSet.map (fun eq_class -> gen_pairs eq_class) eq_classes in 
  let eq_pairs = BatSet.fold (fun eq_pairs acc -> BatSet.union eq_pairs acc) eq_pairs_set BatSet.empty in
  let pc' = BatSet.fold (fun (sv1, sv2) pc -> 
    if BatSet.mem (EQop (Eq, sv1, sv2)) pc || BatSet.mem (EQop (Eq, sv2, sv1)) pc then pc else BatSet.add (EQop (Eq, sv1, sv2)) pc
  ) eq_pairs pc in
  pc'

(*
  Manual Pattern Pruning (* TODO *)
  1.   
    (-1 <= #A (1)
    (#A (2) <= 1)
    (6 + #A (1)) = #A (2)
  2.
   a1 = a2 
   a1 = -a2
  3.
   4 + a1 = a2
   -a1 = a2 - 3 
*)

let rec manual_filter : path_cond -> bool
= fun pc ->
  if BatSet.exists (fun sv -> 
    match sv with
    | ABop (Le, Int n1, ASymbol a1) ->
      BatSet.exists (fun sv' ->
        match sv' with
        | ABop (Le, ASymbol a2, Int n2) ->
          BatSet.exists (fun sv'' -> 
            match sv'' with
            | EQop (Eq, Aop (Add, Int n3, ASymbol a1'), ASymbol a2') when (a1 = a1') && (a2 = a2') -> 
              let t = if (n1 + n3) > n2 then true else false in
              t
            | _ -> false
          ) pc
        | _ -> false
      ) pc 
    | EQop (Eq, sv1, sv2) ->
      begin match sv1, sv2 with
      | Aop (Add, Int n1, ASymbol a1), ASymbol a2 ->
        BatSet.exists (fun sv' ->
          match sv' with
          | EQop (Eq, sv1', sv2') ->
            begin match sv1', sv2' with
            | Minus (ASymbol a1'), Aop (Sub, ASymbol a2', Int n2) -> (n1 + n2) mod 2 <> 0
            | _ -> false 
            end
          | _ -> false
        ) pc
      | _ ->
        BatSet.exists (fun sv' ->
          match sv' with
          | EQop (Eq, Minus sv1', sv2') | EQop (Eq, sv2', Minus sv1') -> ((sv1 = sv1') && (sv2 = sv2')) || (sv1 = sv2' && sv2 = sv1')
          | _ -> false
        ) pc
      end
    | _ -> false
  ) pc then false else true

let normalize_formula2 : sym_formula -> sym_formula
= fun psi -> 
  psi 
  |> BatSet.map (fun (pc, sv) -> (normalize_pc pc BatSet.empty, sv))
  |> BatSet.map (fun (pc, sv) -> (normalize_pc2 pc pc, sv))
  |> BatSet.map (fun (pc, sv) -> (agument_pc pc pc, sv))
  |> BatSet.map (fun (pc, sv) -> (update_eq_class pc, sv))
  |> BatSet.map (fun (pc, sv) -> (normalize_pc pc BatSet.empty, sv))
  |> BatSet.map (fun (pc, sv) -> (BatSet.map (fun sv -> reorder_sym_val sv) pc, reorder_sym_val sv))


let filter_formula2 : sym_formula -> sym_formula
= fun psi -> psi 
  |> BatSet.filter (fun (pc, sv) -> is_sat_pc pc pc)
  |> BatSet.filter (fun (pc, sv) -> is_sat_pc2 pc pc) 
  (*|> BatSet.filter (fun (pc, sv) -> manual_filter pc) *)
  (*|> BatSet.filter (fun (pc, sv) -> filter_unsat_class pc)*)

let rec run2 : sym_formula -> sym_formula
= fun psi ->
  let one_step x = 
    let x = normalize_formula2 x in
    let x = filter_formula2 x in
    x
  in
  one_step psi
