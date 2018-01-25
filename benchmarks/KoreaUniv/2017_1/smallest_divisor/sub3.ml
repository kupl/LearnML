(*2014210080 Choi Kyuhyeon*)
(*Problem 2*)
let rec sqrt_int guess x =
if (guess*guess-x)>0 then guess - 1
else sqrt_int (guess+1) x ;;
  
let rec div x n =
if (n mod x)=0 then x
else if (x < (sqrt_int 1 n)) then div (x+2) n
else n;;

let smallest_divisor n =
match n with
|0 -> 1
|1 -> 1
|_ -> if (n mod 2)=0 then 2
else div 3 n;;