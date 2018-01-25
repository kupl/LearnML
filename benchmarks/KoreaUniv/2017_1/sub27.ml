(*problem 1 *) let rec fastexpt: int -> int ->int=fun b n ->
 if n=0 then 1
else if n mod 2=0 then fastexpt b (n/2) * fastexpt b (n/2)
else b * fastexpt b (n-1)
(*problem 2 *) let rec div: int->int->int=fun a n-> if a*a <=n then (if n mod a=0 then a else div (a+1) n) else n
let smallest_divisor: int ->int =fun n -> div 2 n 
(*problem 3*) let rec mul f n= fun a -> 
if n= 0 then  (fun x->x) a
else mul f (n-1) (f a)
  let iter: int*(int->int)->(int->int)=fun (n,f)->mul f n
(*problem 4*) let rec product: (int->int) ->int->int->int=fun f a b -> 
if a= b then f a
else (f b) *  product f a (b-1)

(* problem 5*) let dfact: int ->int =fun n->
if n=0 then 1
  else  if n mod 2= 0 then product (fun x ->2*x) 1 (n/2)
else product (fun x-> 2*x-1) 1 ((n+1)/2)

(* problem 6*) let rec drop: 'a list ->int -> 'a list= fun l n->
if n >0 then 
match l with
|[] -> []
|hd::tl -> drop tl (n-1)
else l
(*problem 7*)
let fst (x,_) = x;;
let snd (_,x) = x;;
let rec  unzip: ('a *'b) list -> 'a  list * 'b list=fun lst ->
match lst with 
[] -> ([],[])
|hd::tl -> let lst1, lst2 =unzip tl in [fst hd] @ lst1, [snd hd] @ lst2
(*problem 8*)
let rec reverse: int list ->int list =fun lst-> match lst with
| []->[]
|hd::tl -> reverse tl @ [hd]
let rec remove: int->int list ->int list=fun a lst-> match lst with
[]->[]
|hd::tl -> if hd= a then tl else hd::(remove a tl)
let rec change: int list->int->int= fun coins amount->
if amount=0 then 1
else if amount < 0 then 0
else match reverse coins with
[]->0
|hd::tl-> change coins (amount-hd) + change (remove hd coins) amount

