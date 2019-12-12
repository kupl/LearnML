
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec length = fun lst ->
  match lst with
  | [] -> 0
  | hd::tl -> 1 + length tl

  let hd = fun lst -> 
  match lst with
  | hd::tl -> hd

  let tl = fun lst ->
  match lst with
  | hd::tl -> tl

    let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Power(var1, int1) -> if(var1 = var) then (if (int1 == 2) then Times[Const int1; Var var1] 
                                                 else Times[Const int1; Power(var1, int1 - 1)])
                           else Const 0
  | Const (int1) -> Const 0
  | Var (var1) -> if(var1 = var) then Const 1 else Const 0
  | Sum(lst) -> let rec protoSum = fun tolst fromlst var ->
  match fromlst with
    | [] -> Sum(tolst)
    | hd::tl -> protoSum (tolst@[diff(hd, var)]) tl var
  in protoSum [] lst var

  | Times(lst) -> let rec protoTime = fun tolst fromlst var pivot ->
    if(pivot = 0) then Sum(tolst) 
    else protoTime (tolst@[Times(diff (hd fromlst, var)::(tl fromlst))]) 
                      ((tl fromlst) @ [hd fromlst]) var (pivot - 1)
  in protoTime [] lst var (length lst)