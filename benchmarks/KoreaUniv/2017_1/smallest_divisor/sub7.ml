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