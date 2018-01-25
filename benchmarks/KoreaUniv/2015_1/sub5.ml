(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> (* TODO *)
  if x < 0 then raise (Failure "error")
  else if y < 0 then raise (Failure "error")
  else if x < y then raise (Failure "error")
  else
  (
    match y with
      | 0 -> 1
      | _ -> 
        if y = x then 1
        else pascal(x-1, y-1) + pascal(x-1, y)    
  )
  

  
(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> (* TODO *)
  if a = b then f(a)
  else (sigma(f) (a+1) b) + f(a)



(* Problem 3 *)
let rec length l = 
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl
  
let rec max : int list -> int
=fun l -> (* TODO *)  
  match l with
  | [] -> raise (Failure "error")
  | hd::tl -> 
    if (length l) = 1 then hd
    else
    (
      let a = max(tl) in
        if(hd > a) then hd
        else a      
    )
      
let rec min : int list -> int
=fun l -> (* TODO *)
  match l with
  | [] -> raise (Failure "error")
  | hd::tl -> 
    if (length l) = 1 then hd
    else
    (
      let a = min(tl) in
        if(hd > a) then a
        else hd
    )



(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> (* TODO *)
  match f with 
  | True -> true
  
  | False -> false
        
  | Neg f ->
    if (f = True) then false
    else true
    
  | Or (f1, f2) ->
    if (f1 = False) then
    (
      if(f2 = False) then false
      else true
    )
    else true
    
  | And (f1, f2) ->
    if (f1 = True) then
    (
      if(f2 = True) then true
      else false
    )
    else false
    
  | Imply (f1, f2) ->
    if (f1 = True) then
    (
      if (f2 = True) then true
      else false
    )
    else true
    
  | Equiv (f1, f2) ->
    if (f1 = f2) then true
    else false


(* Problem 5 *)
type nat = ZERO | SUCC of nat  
  
let rec natLength : nat -> nat -> int
 =fun n1 n2 ->
  if n1 = n2 then 0
  else (natLength n1 (SUCC(n2))) + 1
  
let rec makeNat : int -> nat
=fun n ->
  if n = 0 then ZERO
  else SUCC(makeNat(n-1))

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> (* TODO *)
  makeNat((natLength n1 ZERO) + (natLength n2 ZERO))  

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> (* TODO *)
  makeNat((natLength n1 ZERO) * (natLength n2 ZERO))
