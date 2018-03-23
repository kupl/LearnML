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