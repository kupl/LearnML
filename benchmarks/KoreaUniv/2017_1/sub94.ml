let rec fastexpt b n =
if (n = 0) then 1
else if (n mod 2) = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else b * (fastexpt b (n-1));;

let sd a =
   let i = 2 in 
   let min = ref 0 in
  if (a = 1) then 1
  else
  while float_of_int(i) <= sqrt(float_of_int a) do
    if (a mod i) = 0 then min := !i;
i := !i+1;
  done;;

let rec iter n f =
if (n = 0) then fun x -> x
else fun x -> iter (n-1) f(f);;

let rec product f a b =
if (a = b) then fun x -> f(a)
  else fun x -> f(a) * product f(x) (a+1) b;;

let rec dfact a =
if (a <= 2) then a
else a * dfact (a-2);;

let rec drop a b =
if b = 0 then a
else drop (List.tl a) (b-1);;

