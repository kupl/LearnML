(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  | Empty -> Empty
  | Node (root, left, right) -> Node (root, mirror right, mirror left)

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC x -> natadd x (SUCC n2)

let natmul : nat -> nat -> nat
= fun n1 n2 ->
  let rec loop = fun cnt acc ->
    match cnt with
    | ZERO -> acc
    | SUCC x -> loop x (natadd acc n2)
  in
  loop n1 ZERO

let natexp : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | _ ->
    let rec loop = fun cnt acc ->
      match cnt with
      | ZERO -> acc
      | SUCC x -> loop x (natmul acc n1)
    in
    loop n2 (SUCC ZERO)

(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f ->
  let extract_var = fun f ->
    let rec append = fun x y ->
      match x with
      | [] -> y
      | hd :: rest -> hd :: (append rest y)
    in
    let rec extract_loop = fun f ->
      match f with
      | Var x -> [(x, false)]
      | Neg f' -> (extract_loop f')
      | And (x, y) -> append (extract_loop x) (extract_loop y)
      | Or (x, y) -> append (extract_loop x) (extract_loop y)
      | Imply (x, y) -> append (extract_loop x) (extract_loop y)
      | Iff (x, y) -> append (extract_loop x) (extract_loop y)
      | _ -> [("", false)]
    in
    let rec pack = fun vars acc ->
      let rec find = fun v l ->
        match l with
        | [] -> false
        | ((var, _) :: rest) ->
          if var = v then
            true
          else
            find v rest
      in
      match vars with
      | [] -> acc
      | (var, _) as hd :: rest ->
        if var != "" && find var acc = false then
          pack rest (hd :: acc)
        else
          pack rest acc
    in
    pack (extract_loop f) []
  in
  let rec lookup = fun x vars ->
    match vars with
    | [] -> raise (Failure ("Undefined variable: " ^ x))
    | ((var, value) :: rest) ->
      if var = x then
        value
      else
        lookup x rest
  in
  let rec eval = fun vars e ->
    match e with
    | True -> true
    | False -> false
    | Var x -> lookup x vars
    | Neg f -> not (eval vars f)
    | And (x, y) -> (eval vars x) && (eval vars y)
    | Or (x, y) -> (eval vars x) || (eval vars y)
    | Imply (x, y) ->
      (match ((eval vars x), (eval vars y)) with
      | (false, _) -> true
      | (true, true) -> true
      | (true, false) -> false)
    | Iff (x, y) -> eval vars (And(Imply (x, y), Imply (y, x)))
  in
  let rec next = fun l ->
    match l with
    | [] -> []
    | (var, value) :: rest ->
      if value = false then
          (var, true) :: rest
      else
          (var, false) :: next rest
  in
  let rec is_last = fun l ->
    match l with
    | [] -> true
    | (_, value) :: rest ->
      if value = false then
        is_last rest
      else
        false
  in
  let rec solve = fun vars f ->
    if eval vars f = true then
      true
    else
      let next_vars = next vars
      in
        if is_last next_vars then
          false
        else
          solve next_vars f
  in
  solve (extract_var f) f

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
  let rec do_diff = fun (e, x) ->
    match e with
    | Const n -> Const 0
    | Var x' ->
      if x = x' then
        Const 1
      else
        Const 0
    | Power (x', n) ->
      if x' = x then
        match n with
        | 1 -> Const 1
        | 0 -> Const 0
        | _ -> Times [Const n; Power (x', n - 1)]
      else
        Const 0
    | Times l ->
      let rec remove_at = fun ll n ->
        match ll with
        | [] -> []
        | (hd :: rest) ->
          if n = 0 then
            rest
          else
            hd :: remove_at rest (n - 1)
      in
      let rec times_loop = fun ll idx ->
        match ll with
        | [] -> []
        | (hd :: rest) ->
          Times (do_diff (hd, x) :: remove_at l idx) :: times_loop rest (idx + 1)
      in
      Sum (times_loop l 0)
    | Sum l ->
      let rec sum_loop = fun l ->
        match l with
        | [] -> []
        | (hd :: rest) -> (do_diff (hd, x)) :: (sum_loop rest)
      in
      Sum (sum_loop l)
  in
  let rec compress = fun e ->
    let rec add = fun e acc ->
      if e = Const 0 then
        acc
      else
        match acc with
        | [] -> [e]
        | a :: rest ->
          match e, a with
          | Const c1, Const c2 -> Const (c1 + c2) :: rest
          | Var x1, Var x2 ->
            if x1 = x2 then
              Times [Const 2; Var x1] :: rest
            else
              Var x1 :: Var x2 :: rest
          | _, _ -> e :: a :: rest
    in
    let rec mult = fun e acc ->
      match acc with
      | [] -> [e]
      | a :: rest ->
        match e, a with
        | Const c1, Const c2 -> Const (c1 * c2) :: rest
        | Const c, (_ as x)
        | (_ as x), Const c ->
          if c = 1 then
            x :: rest
          else
            e :: a :: rest
        | Var x1, Var x2 ->
          if x1 = x2 then
            Power (x1, 2) :: rest
          else
            Var x1 :: Var x2 :: rest
        | Power (x1, n), Var x2
        | Var x2, Power (x1, n) ->
          if x1 = x2 then
            Power (x1, n + 1) :: rest
          else
            Var x2 :: Power (x1, n) :: rest
        | Power (x1, n1), Power (x2, n2) ->
          if x1 = x2 then
            Power (x1, n1 + n2) :: rest
          else
            Power (x1, n1) :: Power (x2, n2) :: rest
        | _, _ -> e :: a :: rest
    in
    let rec compress_sum = fun s acc ->
      match s with
      | [] -> acc
      | e :: rest ->
        let ce = compress e in
        compress_sum rest (add ce acc)
    in
    let rec compress_times = fun t acc ->
      match t with
      | [] -> acc
      | e :: rest ->
        let ce = compress e in
        if ce = Const 0 then
          []
        else
          compress_times rest (mult ce acc)
    in
    let reverse = fun l ->
      let rec reverse_loop = fun acc ll ->
        match ll with
        | [] -> acc
        | hd :: tl -> reverse_loop (hd :: acc) tl
      in
      reverse_loop [] l
    in
    match e with
    | Sum e' ->
      (let ce = compress_sum e' [] in
      match ce with
      | [] -> Const 0
      | [x] -> x
      | _ -> Sum (reverse ce))
    | Times e' ->
      (let ce = compress_times e' [] in
      match ce with
      | [] -> Const 0
      | [x] -> x
      | _ -> Times (reverse ce))
    | Power (x, n) ->
      (match n with
      | 0 -> Const 1
      | 1 -> Var x
      | _ -> Power (x, n))
    | _ -> e
  in
  compress (do_diff (e, x))

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
  let rec eval = fun e x ->
    match e with
    | X -> x
    | INT n -> n
    | ADD (a, b) -> (eval a x) + (eval b x)
    | SUB (a, b) -> (eval a x) - (eval b x)
    | MUL (a, b) -> (eval a x) * (eval b x)
    | DIV (a, b) -> (eval a x) / (eval b x)
    | SIGMA (start', end', body) ->
      let rec loop = fun i e ->
        if i <= e then
          (eval body i) + loop (i + 1) e
        else
          0
      in
      loop (eval start' x) (eval end' x)
  in
  eval e 0

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec wt = fun b ->
    match b with
    | SimpleBranch (_, w) -> w
    | CompoundBranch (_, (left, right)) -> (wt left) + (wt right)
  in
  let ln = fun b ->
    match b with
    | SimpleBranch (l, _) -> l
    | CompoundBranch (l, _) -> l
  in
  let (left, right) = m in
  ((wt left) * (ln left)) = ((wt right) * (ln right))

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec bindec = fun b acc ->
    match b with
    | [] -> acc
    | ZERO :: rest -> bindec rest (acc * 2)
    | ONE :: rest -> bindec rest (acc * 2 + 1)
  in
  let decbin = fun d ->
    let reverse = fun l ->
      let rec reverse_loop = fun acc ll ->
        match ll with
        | [] -> acc
        | hd :: tl -> reverse_loop (hd :: acc) tl
      in
      reverse_loop [] l
    in
    let rec decbin_loop = fun d ->
      if d = 0 then
        []
      else
        match d mod 2 with
        | 0 -> ZERO :: decbin_loop (d / 2)
        | _ -> ONE :: decbin_loop (d / 2)
    in
    let result = reverse (decbin_loop d)
    in match result with
    | [] -> [ZERO]
    | result -> result
  in
  decbin ((bindec b1 0) * (bindec b2 0))
