(*1*)
let rec fastexpt : int->int->int = fun b n -> if n=0 then 1 else if n=1 then b else if n mod 2 = 1 then b*fastexpt b (n-1) else (fastexpt b (n/2))*(fastexpt b (n/2));;

(*2*)
let rec odd_divisor n t = if n mod t = 0 then t else odd_divisor n (t+1);;
let smallest_divisor : int->int = fun n ->
if n mod 2=0 then 2 else odd_divisor n 2;;

(*3*)
let rec iter : int * (int->int)->(int->int)=fun (n,f)->
if n>1 then fun x->f(iter((n-1),f) x) else if n=1 then  fun x->f(x) else fun x->x;; 

(*4*)

let rec product : (int->int)->int->int->int = fun f a b->
if a=b then a else b * (product f a (b-1));;

(*5*)
let rec dfact : int->int = fun n->
if n mod 2=0 then(let k=(n/2) in if k>0 then n*dfact(n-2) else 1)    
    else (let j=((n+1)/2) in if j>0 then n*dfact(n-2) else 1);;

(*6*)
let rec drop : 'a list -> int -> 'a list = fun l n ->
if n>0 then (if List.length l > 0 then drop( List.tl l) (n-1) else []) else l;;  

(*7*)
let unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
List.split lst;;

(*8*)

let rec change_help coins m amount =
 let th coins m = List.nth coins m in
  if amount=0 then 1
      else if amount<0 then 0
        else if (m<=0&& amount>=1) then 0
          else ((change_help coins (m-1) amount) +(change_help coins m (amount-(th coins (m-1)))));;

let change : int list->int->int = fun coins amount ->
  let m = List.length coins in
  if amount<0 then 0 else  change_help coins m amount;;

