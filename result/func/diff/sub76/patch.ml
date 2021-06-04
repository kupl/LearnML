type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec check_times ((li : aexp list), (str : string)) : int =
  match li with
  | [] -> 0
  | h :: t -> (
      match h with
      | Var str3 -> if str = str3 then 1 else check_times (t, str)
      | Power (str2, a) -> if str = str2 then 2 else check_times (t, str)
      | _ -> check_times (t, str) )


let rec align_head ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Times h :: t ->
      if h = Var x then Times (h :: t) else align_head (Times (t @ [ h ]), x)
  | _ -> raise Failure "HowDidYouGetHere"


let work_head ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Times h :: t -> Times t
  | _ -> raise Failure "HowDidYouGetHere"


let rec align_head_power ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Times h :: t -> (
      match h with
      | Power (x, _) -> Times (h :: t)
      | _ -> align_head_power (Times (t @ [ h ]), x) )
  | _ -> raise Failure "HowDidYouGetHere"


let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Sum h :: t -> Sum [ diff (h, x); diff (Sum t, x) ]
  | Times __s66 :: __s67 ->
      Sum
        [
          Times (diff (__s66, x) :: __s67);
          Times [ __s66; diff (Times __s67, x) ];
        ]
  | Times li -> (
      match check_times (li, x) with
      | 0 -> Const 0
      | 1 -> work_head (align_head (Times li, x), x)
      | 2 -> work_head_power (align_head_power (Times li, x), x)
      | _ -> Const 0 )
  | Sum [] -> Const 0
  | Const a -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, a) ->
      if str = x then Times [ Const a; Power (str, a - 1) ] else Const 0


and work_head_power ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Times h :: t -> Times ([ diff (h, x) ] @ t)
  | _ -> raise Failure "HowDidYouGetHere"
