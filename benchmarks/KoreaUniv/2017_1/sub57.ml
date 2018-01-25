(*problem 1*)
  let rec fastexpt = 
    fun b n -> if n=0 then 1
               else 
                if (n mod 2)=0 then (fun x->x*x) (fastexpt b (n/2))
                else b*(fastexpt b (n-1));;

(*problem 2*)
  let rec div a b = 
      if a<(b*b) then a
      else
        if (a mod b)=0 then b
        else (div a (b+1));;

  let smallest_divisor n =
      if (n mod 2)=0 then 2
      else (div n 3);;

(*problem 3*)
  let comp f g = fun x -> g (f x);;
  let rec iter =
    fun (n,f) -> if n=0 then fun x->x
                 else 
                  if n=1 then f
                  else comp f (iter(n-1,f));;

(*problem 4*)
  let rec product =
    fun f a b -> if a=b then f(b)
                 else f(a)*(product f (a+1) b);;

(*problem 5*)
  let rec dfact = 
    fun n -> if (n mod 2)=0 then (product (fun x->2*x) 1 (n/2))
             else (product (fun x->2*x-1) 1 ((n+1)/2));;

(*problem 6*)
  let rec drop l n = match l with
                    | []->[]
                    | hd::tl -> if n=0 then l
                                else (drop tl (n-1));;

(*problem 7*)
  let rec unzip lst = match lst with
                    | []->([],[])
                    | (x,y)::tl -> 
                          (match unzip tl with (a,b) -> (x::a,y::b));;

(* problem 8*)
  let rec change coins amount =
    match coins with
    | []->0
    | hd::tl -> if amount=0 then 1
                else if (amount<0) then 0
                else if (amount/hd)=0 then (change tl amount)
                else
                  (change tl amount) + (change coins (amount-hd));;

