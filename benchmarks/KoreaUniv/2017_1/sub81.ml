(*problem 1*)
  let rec fastexpt:int->int->int
  =fun b n->
  if n=0 then 1
  else if n mod 2=0 then (fastexpt (b) (n/2))*(fastexpt (b) (n/2))
  else b*(fastexpt b (n-1));;

(*problem 2*)
  let rec divisor n x=if n mod x=0 then x else divisor n (x+1)
  let smallest_divisor:int->int
  =fun n->
  divisor n 2;;

(*problem 3*)
  let compose (f:int->int) (g:int->int)  (x:int) : int = f (g x);;
  let rec iter:int*(int->int)->(int->int)
  =fun(n,f)->
  if n=0 then fun x->x
  else compose f (iter((n-1),f));;

(*problem 4*)
  let rec product:(int->int)->int->int->int
  =fun f a b->
  if b=a then f a
  else (f b)*(product f a (b-1));;

(*problem 5*)
  let rec dfact:int->int
  =fun n->
  match n with
  |1->1
  |_->(product (fun x->x) 1 n)/dfact (n-1);;

(*problem 6*)
  let rec drop: 'a list->int->'a list
  =fun l n->
  if n=0 then l
  else match l with
  |[]->[]
  |hd::tl->drop tl (n-1);;

(*problem 7*)
  let rec alist l=
  match l with
  |[]->[]
  |(a,b)::tl->[a]@alist tl
  let rec blist ll=
  match ll with
  |[]->[]
  |(a,b)::tl->[b]@blist tl
  let unzip:('a*'b)list->'a list * 'b list
  =fun lst->
  (alist lst, blist lst);;

(*problem 8*)
  let rec change:int list->int->int
  =fun coins amount->
  if amount=0 then 1
  else if amount<0 then 0
  else match coins with
  |[]->0
  |hd::tl->(change tl amount)+(change coins (amount-hd));;

