(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
    | [] -> []
    | hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match (a,b) with
    | ([], []) -> []
    | ([], l) -> l
    | (l, []) -> l
    | (hd1::tl1, hd2::tl2) -> [hd1]@[hd2]@(zipper (tl1, tl2));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
  match n with
    | 0 -> fun x -> x
    | 1 -> f
    | _ -> fun x -> f (iter(n-1, f) x)

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec find_ch : aexp list * string -> bool
=fun (l,x) -> match l with
  | [] -> false
  | hd::tl -> (match hd with
      | Var str -> if str = x then true else find_ch (tl, x)
      | Power (str,_) -> if str = x then true else find_ch (tl, x)
      | _ -> find_ch (tl, x))

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
  | Sum l -> (match l with
      | [] -> Const 0
      | [hd] -> diff (hd,x)
      | hd::tl -> Sum ([diff (hd,x)]@[diff ((Sum tl),x)]))
  | Times l -> if find_ch (l,x) then
                  (match l with
                    | [] -> Const 0
                    | hd::tl -> (match hd with
                        | Const n -> Times ([Const n]@[diff ((Times tl),x)])
                        | Var v -> if v=x then
                            (match tl with
                              | [] -> diff (Var v,x)
                              | _ -> diff (Times (tl@[Var v]),x))
                          else Times ([Var v]@[diff ((Times tl),x)])
                        | Power (v,n) -> if v=x then
                            (match tl with
                              | [] -> diff (Power (v,n),x)
                              | _ -> diff (Times (tl@[Power (v,n)]),x))
                          else Times ([Power (v,n)]@[diff ((Times tl),x)])
                        | _ -> Times l))
                else Const 0
  | Var v -> if v=x then Const 1 else Const 0
  | Power (v,n) -> if v=x then
      (match n with
        | 1 -> Const 1
        | 2 -> Times ([Const 2]@[Var v])
        | _ -> Times ([Const n]@[Power (v,(n-1))]))
    else Const 0
  | Const n -> Const 0;;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec repl_X_int : exp * exp -> exp
=fun (e,n) -> match e with
  | X -> n
  | INT x -> INT x
  | ADD (e1,e2) -> ADD (repl_X_int (e1,n),repl_X_int (e2,n))
  | SUB (e1,e2) -> SUB (repl_X_int (e1,n),repl_X_int (e2,n))
  | MUL (e1,e2) -> MUL (repl_X_int (e1,n),repl_X_int (e2,n))
  | DIV (e1,e2) -> DIV (repl_X_int (e1,n),repl_X_int (e2,n))
  | SIGMA (e1,e2,e3) -> SIGMA (repl_X_int (e1,n),repl_X_int (e2,n), repl_X_int(e3,n))

let rec calculator : exp -> int
=fun e -> match e with
  | X -> raise (Failure "error")
  | INT n -> n
  | ADD (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c + d
      | (INT c, e2) -> c + calculator e2
      | (e1, INT d) -> calculator e1 + d
      | (e1, e2) -> calculator e1 + calculator e2)
  | SUB (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c - d
      | (INT c, e2) -> c - calculator e2
      | (e1, INT d) -> calculator e1 - d
      | (e1, e2) -> calculator e1 - calculator e2)
  | MUL (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c * d
      | (INT c, e2) -> c * calculator e2
      | (e1, INT d) -> calculator e1 * d
      | (e1, e2) -> calculator e1 * calculator e2)
  | DIV (a,b) -> (match (a,b) with
      | (INT c, INT d) -> c / d
      | (INT c, e2) -> c / calculator e2
      | (e1, INT d) -> calculator e1 / d
      | (e1, e2) -> calculator e1 / calculator e2)
  | SIGMA (a, b, e3) -> (match e3 with
      | SIGMA (_,_,_) -> calculator (SIGMA (a, b, INT (calculator e3)))
      | _ -> (match (a,b) with
          | (INT c, INT d) ->
                if c = d then calculator (repl_X_int (e3,INT d))
                else calculator (repl_X_int (e3,INT c)) + calculator (SIGMA ((INT (c+1), INT d, e3)))
          | (INT c, e2) -> calculator (SIGMA (INT c, INT (calculator e2), e3))
          | (e1, INT d) -> calculator (SIGMA (INT (calculator e1), INT d, e3))
          | (e1, e2) -> calculator (SIGMA (INT (calculator e1), INT (calculator e2), e3))))
