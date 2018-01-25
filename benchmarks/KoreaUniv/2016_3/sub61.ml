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

  let diff : aexp * string -> aexp
  = fun (exp, var) ->
      let rec down e =
        match e with
        | Var s -> if s = var then Const 1 else Const 0
        | Power (s, n) -> 
            if s = var then
              match n with
              | 0 -> Const 0
              | 1 -> Const 1
              | _ -> Times [Const n; Power (s, n - 1)]
            else Const 0
        | Const _ | Times [] | Sum [] -> Const 0
        | Times (h :: []) | Sum (h :: []) -> down h
        | Times l ->
            let rec loop p f r =
              match r with 
              | h :: t -> loop (p @ [Times (f @ [down h] @ t)]) (f @ [h]) t
              | _ -> p in
            Sum (loop [] [] l)
        | Sum l ->
            let rec loop p r =
              match r with
              | h :: t -> loop (p @ [down h]) t
              | _ -> p in
            Sum (loop [] l) in
      let rec simplify e =
        match e with
        | Sum l ->
            let rec loop n x r =
              match r with 
              | h :: t -> begin
                  match simplify h with
                  | Const m -> loop (n + m) x t
                  | sh -> loop n (x @ [sh]) t
                end
              | _ -> match (n, x) with
                  | (sn, []) -> Const sn
                  | (0 , sx) -> Sum sx
                  | (sn, sx) -> Sum (sx @ [Const sn]) in
            loop 0 [] l
        | Times l ->
            let rec loop n x r =
              match r with
              | h :: t -> begin
                  match simplify h with
                  | Const m -> loop (n * m) x t
                  | sh -> loop n (x @ [sh]) t
                end
              | _ -> match (n, x) with
                  | (0 , _ ) -> Const 0
                  | (sn, []) -> Const sn
                  | (1 , sx) -> Times sx
                  | (sn, sx) -> Times ((Const sn) :: sx) in
            loop 1 [] l
        | Power (s, 1) -> Var s 
        | Power (s, 0) -> Const 1
        | _ -> e in
      simplify (down exp)
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

  let balanced : mobile -> bool
  = fun mob -> 
      let rec mob_check m = match m with
      | (  SimpleBranch (ll, lw),   SimpleBranch (rl, rw)) -> (ll * lw = rl * rw, lw + rw)
      | (  SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
          let (rb, rw) = mob_check rm in (rb && ll * lw = rl * rw, lw + rw)
      | (CompoundBranch (ll, lm),   SimpleBranch (rl, rw)) ->
          let (lb, lw) = mob_check lm in (lb && ll * lw = rl * rw, lw + rw)
      | (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
          let (lb, lw) = mob_check lm in
          let (rb, rw) = mob_check rm in
          (lb && rb && ll * lw = rl * rw, lw + rw)
      in match mob_check mob with (b, w) -> b
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

  let calculator : exp -> int
  = fun exp -> 
      let castf d = Int64.shift_left (Int64.of_int d) 20 in
      let castd f = Int64.to_int (Int64.shift_right 
                      (Int64.add f (Int64.div (castf 1) 1000L)) 20) in
      let fone = castf 1 in
      let rec calc ep x = match ep with
      | X -> x
      | INT n -> castf n
      | ADD (e1, e2) -> Int64.add (calc e1 x) (calc e2 x)
      | SUB (e1, e2) -> Int64.sub (calc e1 x) (calc e2 x)
      | MUL (e1, e2) -> Int64.mul 
                          (Int64.shift_right (calc e1 x) 10)
                          (Int64.shift_right (calc e2 x) 10)
      | DIV (e1, e2) -> Int64.div
                          (Int64.shift_left  (calc e1 x) 10)
                          (Int64.shift_right (calc e2 x) 10)
      | SIGMA (ef, et, e) ->
          let (yf, yt) = (calc ef x, calc et x) in
          let (f, t) = if yf < yt then (yf, yt) else (yt, yf) in
          let rec loop y res = if y <= t 
            then loop (Int64.add y fone) (Int64.add res (calc e y)) 
            else res in 
          loop f 0L
      in castd (calc exp 0L)
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

  let check : exp -> bool
  = fun exp ->
      let rec contains v vlist = match vlist with
        | h :: t -> if v = h then true else contains v t
        | _ -> false in
      let rec chk e vlist = match e with
        | P (v, ne) -> chk ne (v :: vlist)
        | C (e1, e2) -> (chk e1 vlist) && (chk e2 vlist)
        | V v -> contains v vlist
      in chk exp []
end

