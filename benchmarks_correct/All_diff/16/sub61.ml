
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