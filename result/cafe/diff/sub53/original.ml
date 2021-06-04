type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (v : string)) : aexp =
  match e with
  | Const n -> Const 0
  | Var x -> if x = v then Const 1 else Var x
  | Power (x, i) ->
      if i = 1 then Const 1 else Times [ Const i; Power (x, i - 1) ]
  | Times al ->
      let rec diff_times (el : aexp list) (x : string) : aexp list =
        match el with
        | [] -> []
        | Const c :: tl -> Const c :: diff_times tl x
        | Times ts :: tl -> diff_times (List.append ts tl) x
        | hd :: tl -> diff (hd, v) :: diff_times tl x
      in

      let rec clean_times (rs : aexp list) (coef : int) : aexp list * int =
        match rs with
        | [] -> ([], coef)
        | Const n :: tl -> clean_times tl (coef * n)
        | hd :: tl ->
            let (t0, m) : aexp list * int = clean_times tl coef in
            (hd :: t0, m)
      in

      let rec concat_times (ts : aexp list) : aexp list =
        match ts with
        | [] -> []
        | Times t1 :: tl -> concat_times (List.append t1 tl)
        | hd :: tl -> hd :: concat_times tl
      in

      let t1 : aexp list = diff_times al v in

      let t2 : aexp list = concat_times t1 in

      let (t3, n) : aexp list * int = clean_times t2 1 in
      if n = 1 then Times t3 else Times (Const n :: t3)
  | Sum al ->
      let rec diff_list (el : aexp list) (x : string) : aexp list =
        match el with [] -> [] | hd :: tl -> diff (hd, x) :: diff_list tl x
      in

      let rec clean_sum (rs : aexp list) (con : int) : aexp list * int =
        match rs with
        | [] -> ([], con)
        | Times [ a ] :: tl -> clean_sum (a :: tl) con
        | Const n :: tl -> clean_sum tl (con + n)
        | hd :: tl ->
            let (ts, con1) : aexp list * int = clean_sum tl con in
            (hd :: ts, con1)
      in

      let s1 : aexp list = diff_list al v in

      let (s2, n) : aexp list * int = clean_sum s1 0 in
      if n = 0 then Sum s2 else Sum (List.append s2 [ Const n ])
