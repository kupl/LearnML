(*problem 2*)
  let rec div a b = 
      if a<(b*b) then a
      else
        if (a mod b)=0 then b
        else (div a (b+1));;

  let smallest_divisor n =
      if (n mod 2)=0 then 2
      else (div n 3);;