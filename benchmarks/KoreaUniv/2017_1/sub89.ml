(*problem1*)
let rec fastexpt : int->int->int = fun b n ->
  if n=1 then b
  else
  if n mod 2 = 0 then fastexpt (b*b) (n/2)
  else b*(fastexpt b (n-1));;

(*problem2*)
let rec p2_subfunc : int->int->int->int = fun a p m->
  if p<=m && a mod p = 0 then p
  else if p<=m && a mod p != 0 then p2_subfunc a (p+1) m
  else a
let smallest_divisor : int->int = fun n->
  p2_subfunc n 2 (int_of_float(sqrt(float_of_int(n))));;

(*problem3*)
let rec iter : int*(int->int)->(int->int) = fun (n,f)->
  if n=0 then fun x->x
  else fun x->f(iter (n-1,f) x);;

(*problem4*)
let rec product :(int->int)->int->int->int = fun f a b ->
  if a!=b then
    (f a)*product f (a+1) b
  else
    f b;;

(*problem5*)
let rec dfact : int->int = fun n->
  if n mod 2 = 0 then
    if n=2 then
      n
    else
      n*(dfact (n-2))
  else
    if n=1 then
      n
    else
     n*(dfact (n-2));;

(*problem6*)
let rec drop : 'a list->int->'a list = fun l n ->
  if n=0 then
    l
  else
  match l with
    []->[]
    |hd::tl-> drop tl (n-1);;

(*problem7*)
let p7_subfunc : 'a->'b->'a list*'b list->'a list*'b list = fun x y (lst1,lst2)-> (lst1@[x],lst2@[y])
let rec unzip2 : ('a*'b) list -> 'a list*'b list -> 'a list * 'b list = fun lst (lst1,lst2) ->
	match lst with 
		hd::tl->let (x,y) = hd in
						let tmp = p7_subfunc x y (lst1,lst2) in
						unzip2 tl tmp
    |[]->(lst1,lst2)
let unzip : ('a*'b)list -> 'a list * 'b list = fun lst -> unzip2 lst([],[]);;

(*problem8*)
let rec change : int list->int->int = fun coins amount ->
  match coins with 
  |coin::remains->if amount<0 then 0
                    else if amount=0 then 1
                    else  change coins (amount-coin) + change remains amount
  |_->0;;