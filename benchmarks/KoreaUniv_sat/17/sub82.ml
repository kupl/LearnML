(*problem 3*)
  type formula =
    True
    |False
    |Var of string
    |Neg of formula
    |And of formula * formula
    |Or of formula * formula
    |Imply of formula * formula
    |Iff of formula * formula
  exception Problem;;

  let sat : formula -> bool
  = fun f ->
  let rec repele = fun lst x -> match lst with
  | [] -> false
  | hd::tl -> if x=hd then true else repele tl x in
  let rec arrange = fun lst -> match lst with
    | [] -> []
    | hd::tl -> if (repele tl hd)=false then hd::(arrange tl)
    else arrange tl in
  let rec mkinitlist
  = fun f -> match f with
    | True -> []
    | False -> []
    | Var x -> [(x,false)]
    | Neg x -> mkinitlist x
    | And (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Or (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Imply (f1,f2) -> (mkinitlist f1)@(mkinitlist f2)
    | Iff (f1,f2) -> (mkinitlist f1)@(mkinitlist f2) in
  let rec terminatelist = fun lst -> match lst with
  | [] -> []
  | hd::tl -> (match hd with
    | (x,_) -> ((x,true)::(terminatelist tl))) in
  let rec nextlist = fun lst -> match lst with
  | [] -> []
  | hd::tl -> (match hd with
      | (x,false) -> (x,true)::tl
      | (x,true) -> ((x,false)::(nextlist tl))) in
  let rec find = fun x lst -> match lst with
  | [] -> raise Problem
  | hd::tl -> (match hd with
    | (a,true) -> if a=x then true else find x tl
    | (a,false) -> if a=x then false else find x tl) in
  let rec eval = fun f lst -> match f with
  | True -> true
  | False -> false
  | Var x -> find x lst
  | Neg f1 -> not(eval f1 lst)
  | And (f1,f2) -> (eval f1 lst)&&(eval f2 lst)
  | Or (f1,f2) -> (eval f1 lst)||(eval f2 lst)
  | Imply(f1,f2) -> eval (Or(Neg(f1),f2)) lst
  | Iff(f1,f2) -> eval(Or(And(Neg(f1),Neg(f2)),And(f1,f2))) lst in
  let rec newsat = fun f lst -> if (eval f lst)=true then true
  else if lst=(terminatelist lst) then false else newsat f (nextlist lst) in
  newsat f (arrange (mkinitlist f));;
