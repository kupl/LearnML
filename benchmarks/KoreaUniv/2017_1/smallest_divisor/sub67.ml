let square x = x*x;;

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