open Lang
open Util
open Symbol_lang2

(* Normalize the result of symbolic execution *)
(* Rule based approaches *)
(* Negation *)
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
    | Int n1, Aop (Add, Int n2, sv') | Int n1, Aop (Add, sv', Int n2) -> normalize_sym_val (Aop (Add, Int (n1 + n2), sv'))
    | Aop (Add, Int n1, sv'), Int n2 | Aop (Add, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Add, Int (n1 + n2), sv'))
    | Aop (Sub, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Add, Int (n2 - n1), sv'))
    | _ -> Aop (op, sv1, sv2)
    end
	| Sub -> 
		begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 - n2)
    | Int 0, sv -> Minus sv
    | sv, Int 0 -> sv
    | Int n1, Aop (Sub, Int n2, sv') -> normalize_sym_val (Aop (Add, Int (n1 - n2), sv'))
    | Int n1, Aop (Sub, sv', Int n2) -> normalize_sym_val (Aop (Sub, Int (n1 + n2), sv'))
    | Aop (Sub, Int n1, sv'), Int n2 -> normalize_sym_val (Aop (Sub, Int (n1 - n2), sv'))
    | Aop (Sub, sv', Int n1), Int n2 -> normalize_sym_val (Aop (Sub, sv', Int (n1 + n2)))
    | sv1, sv2 -> if sv1 = sv2 then Int 0 else Aop (op, sv1, sv2)
    end
  | Mul ->
    begin match (sv1, sv2) with
    | Int n1, Int n2 -> Int (n1 * n2)
    | Int 0, _ | _, Int 0 -> Int 0
    | Int 1, sv | sv, Int 1 -> sv
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
    sv'

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

let is_sub : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Sub, sv1', sv2') -> (sv1' = sv2 && is_int sv2') || (sv2' = sv2 && is_int sv1')
  | _ -> false  

let is_sub2 : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Sub, sv1', sv2') -> (sv1' = sv2 && is_pos sv2') || (sv2' = sv2 && is_pos sv1')
  | _ -> false 

let is_mul : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Mul, sv1', sv2') -> (sv1' = sv2 && is_int sv2') || (sv2' = sv2 && is_int sv1')
  | _ -> false  

let is_div : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 ->
  match sv1 with
  | Aop (Div, sv1', sv2') -> (sv1' = sv2 && is_int sv2') || (sv2' = sv2 && is_int sv1')
  | _ -> false  

let is_aop : symbolic_value -> symbolic_value -> bool
= fun sv1 sv2 -> (is_add sv1 sv2) || (is_sub sv1 sv2) || (is_mul sv1 sv2) || (is_div sv1 sv2)

(* An eq_relation sv is unsat if there exist sv1 = sv2 and sv is sv2 = (sv1 aop const) *)
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

(* An eq_relation sv is unsat if there exist sv1 >= sv2 and sv is sv2 = (sv1 - pos) *)
let comp_sat_checker : comparator -> path_cond -> symbolic_value -> symbolic_value -> bool
= fun op pc sv1 sv2 ->
  match op with
  | Lt | Le -> 
    BatSet.exists (fun sv -> 
      match sv with
      | EQop (Eq, sv1', sv2') -> 
        if sv1' = sv1 && is_sub2 sv2' sv2 then true
        else if sv2' = sv1 && is_sub2 sv1' sv2 then true
        else false
      | _ -> false 
    ) pc
  | Gt | Ge ->
    BatSet.exists (fun sv -> 
      match sv with
      | EQop (Eq, sv1', sv2') -> 
        if sv1' = sv2 && is_sub2 sv2' sv1 then true
        else if sv2' = sv2 && is_sub2 sv1' sv1 then true
        else false
      | _ -> false 
    ) pc

let rec is_sat_pc : path_cond -> path_cond -> bool
= fun pc pc' ->
	if BatSet.is_empty pc then true
	else
		let (sv, pc) = BatSet.pop pc in
		match sv with
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
      else
        begin match op with
        | Lt | Gt -> if (BatSet.mem (ABop (op, sv2, sv1)) pc') || (BatSet.mem (EQop (Eq, sv2, sv1)) pc') || (BatSet.mem (EQop (Eq, sv1, sv2)) pc') then false else is_sat_pc pc pc'
        | _ -> is_sat_pc pc pc'
        end
		| _ -> is_sat_pc pc pc' 

(* Main procedures *)
let normalize_formula : sym_formula -> sym_formula
= fun psi ->
	BatSet.map (fun (pc, sv) -> (normalize_pc pc BatSet.empty, normalize_sym_val sv)) psi
	
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
	let psi = one_step psi in
  psi