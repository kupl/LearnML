(*problem1*)
let rec fastexpt : int -> int -> int = fun b n ->
if (n<0) then raise (Failure "error: n is smaller than 0")
else if n=0 then 1
else if (n mod 2 =0) then (fastexpt b (n/2))*(fastexpt b (n/2))
  else b *(fastexpt b (n-1));;
(*problem2*)
let smallest_divisor :int -> int = fun n->
if (n<0) then raise (Failure "error: n is smaller than 0")
else if (n=0) then 2
else let a=n in
let rec f a i =
if (i*i) > a then a
else if (a mod i) = 0 then i
else f a (i+1) in f a 2;;

(*problem3*)
let rec iter : int *(int -> int) -> (int-> int) = fun (n,f) -> 
if (n<0) then raise (Failure "error :n is smaller than 0")
else if n=0 then fun x -> x
else fun x -> (iter ((n-1),f))(f x);;

(*problem4*)
let rec product : (int ->int)-> int -> int ->int = fun f a b->
if(b<a) then raise (Failure "error : b is smaller than a") 
else if (a=b) then (fun x -> x) (f a)
else (product f b b)*(product f a (b-1));;

(*problem5*)
let rec  dfact : int -> int = fun n ->
if (n<0) then raise (Failure " error : n is smaller than 0")
else if n=0 then 1
else if n=1 then 1
else if n=2 then 2
else n* dfact (n-2);;

(*problem6*)
let rec drop : 'a list->int->'a list = fun l n -> 
if (n<=0) then l
else match l with
|[]->[]
|hd::tl -> if (n=1) then tl
else drop tl (n-1);;

(*problem7*)
let unzip :('a *'b) list -> 'a list *'b list =fun lst ->
let rec un1 l1=
match l1 with
|[]->[]
|hd::tl -> (match hd with (x,_)->x)::(un1 tl)
  in let rec un2 l2 =
  match l2 with 
  |[]->[]
  |hd::tl ->(match hd with (_,x)->x)::(un2 tl)
  in ((un1 lst),(un2 lst));;
(*problem8*)
let rec change : int list -> int -> int = fun coins amount ->
if (amount <0) then 0
else if (amount =0) then 1
else match coins with
|[]->0
|hd::tl -> (change coins (amount -hd))+(change tl (amount));;

