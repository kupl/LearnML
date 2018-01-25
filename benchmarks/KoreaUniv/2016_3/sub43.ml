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
  | Const n -> Const 0
  | Var k -> if k = var then Const 1 else Var k
  | Power (s, n) -> if s = var then if n = 0 then Const (0) else Times [Power (s, n - 1); Const n] else Const 0
  | Times (li) -> let rec timesfun exp var =
    match exp with
    | [] -> []
    | hd::tl -> match hd with 
      | Const n -> [Const n;] @ timesfun tl var
      | Var k -> [diff (hd, var);] @ timesfun tl var
      | Power (s, n) -> if s = var then
         if n = 0 then [Const (0);] @ timesfun tl var
         else [Times [Power (s, n - 1); Const n];] @ timesfun tl var
         else [Const 1;] @ timesfun tl var
      | Times (li) -> [diff (hd, var);] @ timesfun tl var
      | Sum (li) -> [diff (hd, var);] @ timesfun tl var
    in Times (timesfun li var)
  | Sum (li) -> let rec sumfun exp =
    match exp with
    | [] -> []
    | hd::tl -> [diff(hd, var);]@(sumfun tl)
    in Sum (sumfun li)
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

  let rec total_weight : mobile -> int
  = fun mob -> match mob with
  | (b1, b2) -> match b1, b2 with
  | SimpleBranch (len1, wei1), SimpleBranch (len2, wei2) -> wei1 + wei2
  | SimpleBranch (len1, wei1), CompoundBranch (len2, mob2) -> wei1 + total_weight mob2
  | CompoundBranch (len1, mob1), SimpleBranch (len2, wei2) -> total_weight mob1 + wei2
  | CompoundBranch (len1, mob1), CompoundBranch (len2, mob2) -> total_weight mob1 + total_weight mob2
  | _ , SimpleBranch (len2, wei2) -> wei2
  | _ , CompoundBranch (len2, mob2) -> total_weight mob2
  | SimpleBranch (len1, wei1), _ -> wei1
  | CompoundBranch (len1, mob1), _ -> total_weight mob1
  | _ , _ -> 0 (* TODO *)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  | (b1, b2) -> match b1, b2 with
  | SimpleBranch (len1, wei1), SimpleBranch (len2, wei2) -> if len1 * wei1 = len2 * wei2 then true else false
  | SimpleBranch (len1, wei1), CompoundBranch (len2, mob2) -> if len1 * wei1 = len2 * total_weight mob2 && balanced mob2 = true then true else false
  | CompoundBranch (len1, mob1), SimpleBranch (len2, wei2) -> if len1 * total_weight mob1 = len2 * wei2 && balanced mob1 = true then true else false
  | CompoundBranch (len1, mob1), CompoundBranch (len2, mob2) -> if len1 * total_weight mob1 = len2 * total_weight mob2 && balanced mob1 = true && balanced mob2 = true then true else false
  | _ , SimpleBranch (len2, wei2) -> false
  | _ , CompoundBranch (len2, mob2) -> false
  | SimpleBranch (len1, wei1), _ -> false
  | CompoundBranch (len1, mob1), _ -> false
  | _ , _ -> false (* TODO *)
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

  let rec sigma : exp -> int -> int
  = fun exp var -> match exp with
  | X -> var
  | INT n -> n
  | ADD (n1, n2) -> sigma n1 var + sigma n2 var
  | SUB (n1, n2) -> sigma n1 var - sigma n2 var
  | MUL (n1, n2) -> sigma n1 var * sigma n2 var
  | DIV (n1, n2) -> sigma n1 var / sigma n2 var
  | SIGMA (n1, n2, n3) -> if sigma n1 var = sigma n2 var then sigma n3 (sigma n1 var) else
   sigma n3 (sigma n1 var) + sigma (SIGMA (INT (sigma n1 var + 1), n2, n3)) var

  let rec calculator : exp -> int
  = fun exp -> match exp with
  | INT n -> n
  | ADD (n1, n2) -> calculator n1 + calculator n2
  | SUB (n1, n2) -> calculator n1 - calculator n2
  | MUL (n1, n2) -> calculator n1 * calculator n2
  | DIV (n1, n2) -> calculator n1 / calculator n2
  | SIGMA (n1, n2, n3) -> if calculator n1 = calculator n2 then sigma n3 (calculator n1)
   else sigma n3 (calculator n1) + calculator (SIGMA (INT (calculator n1 + 1), n2, n3))
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

  let rec set_var : exp -> var list -> bool
  = fun exp str -> match exp with 
  | V r -> if List.mem r str then true else false
  | P (aexp, e) -> set_var e ([aexp;]@str)
  | C (e1, e2) -> set_var e1 str && set_var e2 str

  let rec check : exp -> bool
  = fun exp -> match exp with
  | V str -> false
  | P (str, e) -> set_var e [str;]
  | C (e1, e2) -> check e1 && check e2 (* TODO *)
end

