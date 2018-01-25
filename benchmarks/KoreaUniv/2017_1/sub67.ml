let square x = x*x;;

let rec fastexpt b n=
if(n=0) 
  then 1
  else
  if(n mod 2=0)
  then square(fastexpt b (n/2))
  else b*(fastexpt b (n-1));;

let rec divisor n a=
if(1<a)
  then
    if(n mod a=0)
     then a
     else divisor n (a-1)
  else  n;;

let is_good_enough guess x=
abs_float(guess *. guess -. x)<0.001;;

let improve guess x=(guess +. x/.guess)/. 2.0;;

let rec sqrt_iter guess x=
 if is_good_enough guess x then guess
 else sqrt_iter (improve guess x) x;;

let sqrt x=sqrt_iter 1.0 x;;

let smallest_divisor n=
divisor n (int_of_float (sqrt (float_of_int n)));;

let compose f g=fun x->f(g x);;

let rec iter :int*(int->int)->(int->int)
=fun (n,f)->
if(n=0) then fun x->x
else
  if(n>1) then compose f (iter((n-1),f))
  else f;;

let rec product: (int->int)->int->int->int
=fun f a b ->if(a=b) then f a
else (f (a))*(product f (a+1) b);; 

let rec dfact:int->int
=fun n->if(n mod 2=0) then (product (fun x->x) 1 (n/2))*(fastexpt 2 (n/2))
  else ((product (fun x->x) 1 n)/(dfact (n-1)));;

let rec length l=
match l with
|[]->0
|hd::tl->1+length tl;;

let rec drop: 'a list->int->'a list
=fun l n ->
if(n=0) then l
else
match l with
|[]->[]
|hd::tl-> drop tl (n-1);;

let rec length l=
match l with
|[]->0
|hd::tl->1+length tl;;

let first l=
match l with
|[]->[]
|hd::tl->hd;;

let insert_list a l=
match l with
|[]->[a]
|hd::tl->l@[a];;

let rec nth l n=
match l with
|[]->raise(Failure "list is too short")
|hd::tl->if(n=0) then hd else nth tl (n-1);;

let fst p=match p with (x,_)->x;;
let scd p=match p with (_,x)->x;;

let rec mklst1: ('a*'b) list ->'a list
=fun lst->
match lst with
|[]->[]
|hd::tl->(insert_list (fst hd) [])@(mklst1 tl);;

let rec mklst2: ('a*'b) list->'b list
=fun lst->
match lst with
|[]->[]
|hd::tl->(insert_list (scd hd) [])@(mklst2 tl);;


let unzip:('a*'b) list ->'a list*'b list
=fun lst->((mklst1 lst),(mklst2 lst));;

let reduce_list l=
match l with 
|[]->[]
|hd::tl->tl;;

let list_first l=
match l with
|[]->raise(Failure "too much")
|hd::tl->hd;;

let rec length l=
match l with
|[]->0
|hd::tl->1+(length tl);;

let rec change : int list->int->int
=fun coins amount->
if(amount>0) then
  if((length coins)>0) then
    if((amount-(list_first coins))>0) then
    ( change coins (amount-(list_first coins)))+(change (reduce_list coins) amount)else
    if((amount-(list_first coins))<0) then 0
     else 1
else 0
else
  if(amount<0) then 0
  else 1;;
