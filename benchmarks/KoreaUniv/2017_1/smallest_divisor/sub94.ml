let sd a =
   let i = 2 in 
   let min = ref 0 in
  if (a = 1) then 1
  else
  while float_of_int(i) <= sqrt(float_of_int a) do
    if (a mod i) = 0 then min := !i;
i := !i+1;
  done;;