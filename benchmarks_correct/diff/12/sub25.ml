type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let remove_nth l n =
  let rec remove l n c = 
    if c = n then List.tl l
    else (List.hd l)::(remove (List.tl l) n (c+1))
  in
  (remove l n 0)

let rec diff (aexp,str) = 
  let diff_times(l,v) =
    let rec diff_times'(l,v,cur) = 
      if cur > (List.length l) - 1 then []
      else Times(diff(List.nth l cur,v)::(remove_nth l
      cur))::(diff_times'(l,v,cur+1))
    in
    Sum(diff_times'(l,v,0))
  in
  let diff_sum(l,v) = 
    let rec ds(l,v) = 
      match l with
      | [] -> []
      | hd::tl -> diff(hd,v)::ds(tl,v)
    in
    Sum(ds(l,v))
  in
  match aexp with
  | Const i -> Const 0
  | Var s -> if s = str then Const 1 else Const 0
  | Power (s,i) -> 
      if s = str 
      then Times(Const i::[Power (s, i-1)])
      else Const 0
  | Times l -> diff_times(l,str)
  | Sum l -> diff_sum(l,str)
(*
 * modify **
let aexp1 = Times((Var "a")::(Power("x",2))::[])
let aexp2 = Times(Var "b"::Var "x"::[])
let aexp3 = Var "c"
let f1 = Sum(aexp1::aexp2::aexp3::[])
*)
