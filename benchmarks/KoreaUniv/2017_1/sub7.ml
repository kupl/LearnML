let rec fastexpt b n =
  if n=0 then 1
    else if (n mod 2)=1 then b*(fastexpt b (n-1))
    else (fastexpt b (n/2))*(fastexpt b (n/2))
;;

 let smallest_divisor n =
  let i = ref 2 in
   let min = ref 0 in
     while float_of_int(!i) <= sqrt(float_of_int n) do
       if n mod !i = 0 then min := !i;
       if !min = !i then i := !i +n;
       if float_of_int(!i) = sqrt(float_of_int n) then min:= n;
       i:= !i +1
     done;
 if n=2 then 2
 else if n=3 then 3
 else if !min=0 then n
 else !min;;

  let rec iter n f =
    if n=0 then fun x-> x
      else fun x -> iter (n-1) f(f(x)) ;;


  let rec product f a b =
    if a=b then f a
      else (f b)*product f a (b-1);;
      
   let dfact n=
     let fun1 x=2*x in
       let fun2 x=2*x -1 in
         if n mod 2 = 0 then product (fun1) 1 (n/2)
    else product (fun2) 1 ((n+1)/2);;

     let rec drop l n =
       match n with
         |0->l
           |_->let leng=List.length l in if leng<=n then [] else drop (List.tl l) (n-1);;
