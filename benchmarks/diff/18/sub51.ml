type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
  | Const a -> Const 0
  | Var myvar -> if myvar=x then Const 1 else Var myvar
  | Power (myvar,i) -> if myvar=x then Times[ Const i; Power(myvar,i-1) ] else Power(myvar,i)
  | Times (hd::tl) -> Sum[ Times[diff(hd,x);Sum tl] ; diff(Sum tl,x) ]
  | Times [] -> Const 0
  (*
  | Times explst -> 
    begin
    match explst with
      | [] -> Const 0 //not exhaustive error 안뜨게끔 추가
      | hd::tl -> Sum[ Times[diff(hd,x);Sum tl] ; diff(Sum tl,x) ]
    end
  *)
  | Sum (hd::tl) -> Sum[ diff(hd,x) ; diff(Sum tl,x) ]
  | Sum [] -> Const 0;;
  (*
  | Sum explst ->
    match explst with
      | [] -> Const 0 //not exhaustive error 안뜨게끔 추가
      | hd::tl -> Sum[ diff(hd,x) ; diff(Sum tl,x) ];; 
  *)
diff(Sum[Power("x",2); Times[Const 2; Var "x"]; Const 1], "x");;
diff(Sum[Power("x",2)],"x");;
diff(Var "x", "x");;
diff(Const 1, "x");;
diff(Sum[Var "x"; Var "x"], "x");;
diff(Sum[Power("x",2); Const 2],"x");;