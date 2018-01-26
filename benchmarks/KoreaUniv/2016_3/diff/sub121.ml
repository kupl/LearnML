(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
    | Const i -> Const 0
    | Var s -> if (s = var) then Const 1 else Const 0
    | Power (s, i) -> if (s = var) then Times [Const i ; Power (s, i-1)] else Const 0
    | Times lst -> begin match lst with 
      | [] -> Const 1 
      | hd::tl -> Times [diff_times_helper (hd, var) ; diff (Times tl, var)];
    end
    | Sum lst -> begin match lst with 
      | [] -> Const 0
      | hd::tl -> Sum [diff (hd, var) ; diff (Sum tl, var)]
    end

  and diff_times_helper (exp, var) =
    match exp with 
      | Const i -> Const i
      | Var s -> if (s = var) then Const 1 else Var s
      | Power (s, i) -> if (s = var) then Times [Const i ; Power (s, i-1)] else Power(s, i)
      | _ -> diff (exp, var)
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let get_length br = 
    match br with 
      | SimpleBranch (length, weight) -> length
      | CompoundBranch (length, mob) -> length

  let rec balanced : mobile -> bool
  = fun mob -> 
    let (left, right) = mob in 
      let (left_balanced, left_weight) = balanced_helper left in
        let (right_balanced, right_weight) = balanced_helper right in 
          let left_length = get_length left in
            let right_length = get_length right in
             left_balanced && right_balanced && left_weight*left_length == right_weight*right_length
  
  and balanced_helper br =
    match br with
      | SimpleBranch (length, weight) -> (true, weight)
      | CompoundBranch (length, mob) -> 
        let (left, right) = mob in 
          let (left_balanced, left_weight) = balanced_helper left in
            let (right_balanced, right_weight) = balanced_helper right in 
              let left_length = get_length left in
                let right_length = get_length right in
                  (left_balanced && right_balanced && left_weight*left_length == right_weight*right_length, left_weight + right_weight);;
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with 
    | X -> raise (Failure "Value for X has not been provided") (* ?? *)
    | INT i -> i
    | ADD (e1, e2) -> calculator e1 + calculator e2
    | SUB (e1, e2) -> calculator e1 - calculator e2
    | MUL (e1, e2) -> calculator e1 * calculator e2
    | DIV (e1, e2) -> calculator e1 / calculator e2
    | SIGMA (e1, e2, e3) -> let result = ref 0 in (* gotta make a ref cos ocaml doesnt update regular values *)
      for i = calculator e1 to calculator e2 do
        result := !result + sigma_helper e3 i
      done;
      !result
  and sigma_helper exp i = match exp with 
    | X -> i 
    | INT i1 -> i1
    | ADD (e1, e2) -> sigma_helper e1 i + sigma_helper e2 i
    | SUB (e1, e2) -> sigma_helper e1 i - sigma_helper e2 i
    | MUL (e1, e2) -> sigma_helper e1 i * sigma_helper e2 i
    | DIV (e1, e2) -> sigma_helper e1 i / sigma_helper e2 i
    | SIGMA (e1, e2, e3) -> let result = ref 0 in
      for i = sigma_helper e1 i to sigma_helper e2 i do
        result := !result + sigma_helper e3 i
      done;
      !result;;
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec append_list list1 list2 = 
  match list1 with
    | [] -> list2
    | hd::tl -> hd :: (append_list tl list2);;

  let rec get_variables exp result =
  match exp with
    | V v -> v :: result
    | P (v, e) -> get_variables e (v :: result); 
    | C (e1, e2) -> append_list (get_variables e1 result) (get_variables e2 result)

  let rec var_in_exp variable exp = 
  match exp with
    | V v -> false 
    | P (v, e) -> if v = variable then true else var_in_exp variable e
    | C (e1, e2) -> var_in_exp variable e1 || var_in_exp variable e2

  let rec check_helper variables exp = 
  match variables with
    | [] -> true
    | hd::tl -> if var_in_exp hd exp = false then false else check_helper tl exp

  let check : exp -> bool
  = fun exp -> 
  let variables = get_variables exp [] in
    check_helper variables exp;;
end