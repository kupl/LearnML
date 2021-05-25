type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

exception Whatdahell

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec zip l x =
  match l with
    [] -> []
  | h::t -> (h, x) :: zip t x

let rec isvarlist x l =
  match l with
    [] -> true
  | (Const n)::t -> isvarlist x t
  | (Var v)::t -> v = x && isvarlist x t
  | (Power (v, n))::t -> if v = x
                          then isvarlist x t
                          else false
  | h::t -> false

let rec diffvarlist_inner x l nums degree consts =
  match l with
    [] -> if nums = 0 && consts=0 then Times [Const degree; Power (x, degree-1)] else Times [Const nums; Const degree; Power (x, degree-1)]
  | (Power (v, n))::t -> diffvarlist_inner x t nums (n+degree) consts
  | (Var v)::t -> diffvarlist_inner x t nums (1+degree) consts
  | (Const n)::t -> diffvarlist_inner x t (n+nums) degree (consts+1)
  | _ -> raise Whatdahell

let diffvarlist x l = diffvarlist_inner x l 0 0 0

let rec othervarlist x l =
  match l with
    [] -> true
  | (Const n)::t -> othervarlist x t
  | (Var v)::t -> v <> x && othervarlist x t
  | (Power (v, n))::t -> v <> x && othervarlist x t
  | h::t -> false

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
    Const n -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, n) -> if v = x then Times [Power (v, n-1); Const n] else Power (v, n)
  | Sum l ->
      if othervarlist x l then Const 0 else Sum (map diff (zip l x))
  | Times l ->
      if isvarlist x l then diffvarlist x l else
        if othervarlist x l then Const 0 else
          match l with
          | [x] -> Const 0
          | _ -> Times (map diff' (zip l x))
  
and diff' ((exp : aexp), (x : string)) : aexp =
  match exp with
    Const n -> Const n
  | Var v -> if v = x then Const 1 else Var v
  | Power (v, n) -> if v = x then Times [Power (v, n-1); Const n] else Power (v, n)
  | Sum l -> 
      if othervarlist x l then Const 0 else Sum (map diff (zip l x))
  | Times l ->
      if isvarlist x l then diffvarlist x l else
        if othervarlist x l then Const 0 else
          match l with
          | [x] -> Const 0
          | _ -> Times (map diff' (zip l x));;
  
(* x^2 + 2*x + 1 *)
diff ((Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]), "x");;
(* 2*x + 2 *)
diff (Sum [Times [Var "x"; Const 2]; Const 2], "x");;
(* 2*x + 2 *)
diff (Sum [Times [Const 2; Power ("x", 1)]; Sum [Times [Const 0; Var "x"]; Times [Const 2; Sum [Times [Const 1]; Times [Var "x"; Const 0]]]]; Const 0], "x");;
(* 2*(2+x)*2 *)
diff (Times [Const 2; Sum [Const 2; Var "x"]; Const 2], "x");;
(* 2*(2+x)*y *)
diff (Times [Const 2; Sum [Const 2; Var "x"]; Var "y"], "x");;
(* x^2 + 2xy + y^2 *)
diff (Sum [Times [Var "x"; Const 1; Var "x"]; Times [Const 2; Var "x"; Var "y"]; Times [Var "y"; Var "y"]], "x");;
(* y + 2 *)
diff (Sum [Var "y"; Const 2], "x");;
(* y + 2 + 2y^2 *)
diff (Sum [Var "y"; Const 2; Times [Power ("y", 2); Const 2]], "x");;
(* y + 2 + 2y^2 *)
diff (Sum [Var "y"; Const 2; Times [Power ("y", 2); Const 2]], "y");;