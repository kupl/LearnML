type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  let rec times_diff_2 = fun l o r ->
    match o with
      [] -> r
      | hd::tl -> if hd=l then diff(hd,x)::r else hd::r
  in
  let rec times_diff_1 = fun l o r ->
    match l with
      [] -> r
      |hd::tl -> times_diff_1 tl o (Times(times_diff_2 hd o [])::r)
  in
  let rec sum_diff = fun l r ->
    match l with
      []-> r
      | hd::tl -> sum_diff tl (diff(hd,x)::r)
  in
  match exp with
    Const(s) -> Const(0)
    | Var(s) -> if s=x then Const(1) else Const(0)
    |Power(s, i) -> if s=x then if i<>0 then Times[Const(i);Power(s, i-1)] else Const(0) else Const(0)
    |Times(l) -> Sum(times_diff_1 l l [])
    |Sum(l) -> Sum(sum_diff l [])
  ;;
  
diff(Sum[Power("x",2);Times[Power("y",2);Var("x")]], "x");;
