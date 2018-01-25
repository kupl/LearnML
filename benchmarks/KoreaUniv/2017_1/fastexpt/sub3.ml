(*2014210080 Choi Kyuhyeon*)

(*Problem 1*)
let rec fastexpt b n =
match n with
|1 -> b
|_ -> if (n mod 2)=0
then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1));;