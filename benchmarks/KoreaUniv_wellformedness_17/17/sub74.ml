(**********************)
(*   Problem 1        *)
(**********************)

type exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp =
  fun exp ->
  let empty_mem = [] in
    let expand_mem = fun (v, e) mem -> (v, e) :: mem in
      let rec apply_mem =
        fun v mem ->
        match mem with
        [] -> VAR v
        | (p, q) :: tl ->
          if v = p then q else apply_mem v tl
      in
        let rec make_new_mem =
          fun exp mem ->
          match exp with
          ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV(e1, e2) | CALL (e1, e2) ->
            let mem' = make_new_mem e1 mem in
              make_new_mem e2 mem'
          | ISZERO e -> make_new_mem e mem
          | IF (e1, e2, e3) ->
            let mem1 = make_new_mem e1 mem in
              let mem2 = make_new_mem e2 mem1 in
                make_new_mem e2 mem2
          | PROC (x, e) -> make_new_mem e mem
          | LET (x, e1, e2) ->
            let mem' = expand_mem (x, e1) mem in
              make_new_mem e2 mem'
          | LETREC (f, x, e1, e2) ->
            let mem' = expand_mem (f, e1) mem in
              make_new_mem e2 mem'
          |_ -> mem
        in
          let rec check =
            fun exp ->
            let rec get =
              fun exp lst1 lst2 ->
              match exp with
              CONST n -> (lst1, lst2)
              | VAR x -> (lst1, x :: lst2)
              | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV(e1, e2) | CALL (e1, e2) ->
                let t = get e1 lst1 lst2 in
                  get e2 (fst t) (snd t)
              | IF (e1, e2, e3) ->
                let t1 = get e1 lst1 lst2 in
                  let t2 = get e2 (fst t1) (snd t1) in
                    get e3 (fst t2) (snd t2)
              | LET (x, e1, e2) ->
                let lst' = x :: lst1 in
                  get e2 lst' lst2
              | LETREC (f, x, e1, e2) ->
                let lst' = f :: lst1 in
                  get e2 lst' lst2
              | READ -> (lst1, lst2)
              | ISZERO e -> get e lst1 lst2
              | PROC (x, e) -> get e lst1 lst2
            in
              let rec search =
                fun lst1 lst2 ->
                match lst1 with
                [] -> true
                | hd :: tl ->
                  let rec find p q =
                    (match q with
                    [] -> false
                    | hd :: tl ->
                      if hd = p then true
                      else find p tl) in
                  if find hd lst2 then search tl lst2
                  else false
              in
                let t = get exp [] [] in
                  search (fst t) (snd t)
          in
            let rec expand_with_mem =
              fun exp mem ->
              match exp with
              CONST n -> CONST n
              | VAR v -> apply_mem v mem
              | ADD (e1, e2) -> ADD(expand_with_mem e1 mem, expand_with_mem e2 mem)
              | SUB (e1, e2) -> SUB(expand_with_mem e1 mem, expand_with_mem e2 mem)
              | MUL (e1, e2) -> MUL(expand_with_mem e1 mem, expand_with_mem e2 mem)
              | DIV (e1, e2) -> DIV(expand_with_mem e1 mem, expand_with_mem e2 mem)
              | ISZERO e -> ISZERO (expand_with_mem e mem)
              | READ -> let n = read_int() in CONST n
              | IF (e1, e2, e3) -> IF (expand_with_mem e1 mem, expand_with_mem e2 mem, expand_with_mem e3 mem)
              | LET (x, e1, e2) ->
                if check exp then expand_with_mem e2 mem
                else LET (x, e1, expand_with_mem e2 mem)
              | LETREC (f, x, e1, e2) ->
                if check exp then expand_with_mem e2 mem
                else LETREC (f, x, e1, expand_with_mem e2 mem)
              | PROC (x, e) -> PROC (x, expand_with_mem e mem)
              | CALL (e1, e2) -> CALL (expand_with_mem e1 mem, expand_with_mem e2 mem)
            in
              let new_mem = make_new_mem exp empty_mem in
                expand_with_mem exp new_mem
        

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool =
  fun lam ->
  let rec get =
    fun lam lst1 lst2->
    match lam with
    V x -> (lst1, x :: lst2)
    | P (x, e) ->
      let lst' = x :: lst1 in
        get e lst' lst2
    | C (e1, e2) ->
      let t = get e1 lst1 lst2 in
        get e2 (fst t) (snd t)
  in
    let rec search =
      fun lst1 lst2 ->
      match lst2 with
      [] -> true
      | hd :: tl ->
        let rec find p lst =      
          (match lst with
          [] -> false
          | hd :: tl ->
          if hd = p then true
          else find p tl) in
            if find hd lst1 then search lst1 tl
            else false
    in
      match lam with
      V x -> true
      |_ ->
        let t = get lam [] [] in
          search (fst t) (snd t)