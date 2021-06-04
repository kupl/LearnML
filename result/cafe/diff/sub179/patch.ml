type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec num_x (lst : aexp list) (s : string) : int =
  if lst = [] then 0
  else
    match List.hd lst with
    | Times a' -> num_x a' s + num_x (List.tl lst) s
    | Power (a', b') ->
        if a' = s && b' != 0 then b' + num_x (List.tl lst) s
        else 0 + num_x (List.tl lst) s
    | Var a' ->
        if a' = s then 1 + num_x (List.tl lst) s else 0 + num_x (List.tl lst) s
    | Const a' -> if a' = 0 then 0 else 0 + num_x (List.tl lst) s


let rec dif_times_lst (lst : aexp list) (x : string) : aexp list =
  match List.hd lst with
  | Const x' -> [ Const x' ] @ dif_times_lst (List.tl lst) x
  | Var x' ->
      if x' = x then List.tl lst else [ Var x' ] @ dif_times_lst (List.tl lst) x
  | _ -> []


let rec dif_Times (lst : aexp list) (x : string) : aexp list =
  match num_x lst x with
  | 0 -> [ Const 0 ]
  | 1 -> dif_times_lst lst x
  | _ -> [ Const (num_x lst x) ] @ dif_times_lst lst x


let rec dif_sums_lst (lst : aexp list) (x : string) : aexp list =
  match lst with
  | [] -> []
  | _ -> (
      match List.hd lst with
      | Const x' -> [ Const 0 ] @ dif_sums_lst (List.tl lst) x
      | Var x' ->
          if x' = x then [ Const 1 ] @ List.tl lst
          else [ Const 0 ] @ dif_sums_lst (List.tl lst) x
      | Sum a' ->
          if num_x a' x = 0 then [ Const 0 ]
          else dif_sums_lst a' x @ dif_sums_lst (List.tl lst) x
      | Power (a', b') ->
          if a' = x then
            match b' with
            | 1 -> [ Const 1 ] @ dif_sums_lst (List.tl lst) x
            | _ ->
                [ Times [ Const b'; Power (a', b' - 1) ] ]
                @ dif_sums_lst (List.tl lst) x
          else [ Const 0 ] )


let rec dif_all_lst (lst : aexp list) (x : string) : aexp list =
  match lst with
  | [] -> []
  | _ -> (
      match List.hd lst with
      | Const x' -> [ Const 0 ] @ dif_all_lst (List.tl lst) x
      | Var x' ->
          if x' = x then [ Const 1 ] @ List.tl lst
          else [ Const 0 ] @ dif_all_lst (List.tl lst) x
      | Sum a' ->
          if num_x a' x = 0 then [ Const 0 ]
          else dif_sums_lst a' x @ dif_all_lst (List.tl lst) x
      | Power (a', b') ->
          if a' = x then
            match b' with
            | 1 -> [ Const 1 ] @ dif_all_lst (List.tl lst) x
            | _ ->
                [ Times [ Const b'; Power (a', b' - 1) ] ]
                @ dif_all_lst (List.tl lst) x
          else [ Const 0 ]
      | Times a' -> [ Times (dif_Times a' x) ] )


let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const x' -> Const 0
  | Var x' -> if x' = x then Const 1 else Const 0
  | Sum a' -> Sum (List.map (fun (__s64 : aexp) -> diff (__s64, x)) a')
  | Power (a', b') ->
      if a' = x then
        match b' with
        | 1 -> Const 1
        | _ -> Times [ Const b'; Power (a', b' - 1) ]
      else Const 0
  | Times [] -> Const 0
  | Times __s61 :: __s62 ->
      Sum
        [
          Times (diff (__s61, x) :: __s62);
          Times [ __s61; diff (Times __s62, x) ];
        ]
  | Times a' -> Times (dif_all_lst a' x)


let (_ : aexp) = diff (Sum [ Const 2; Times [ Const 2; Var "x"; Var "y" ] ], "x")

let (_ : aexp) = diff (Power ("x", 4), "x")

let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")


let (_ : aexp) =
  diff (Sum [ Power ("x", 4); Times [ Const 2; Var "x" ]; Const 1 ], "x")


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
