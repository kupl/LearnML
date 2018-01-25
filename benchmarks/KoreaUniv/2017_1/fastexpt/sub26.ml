(* problem 1*)
let square x = x*x;; (*helper function*)
let rec fastexpt:int->int->int = fun b n -> if b=0 then 0
else match n with |0->1|_->if n mod 2 = 0 then square (fastexpt b (n/2)) else b*(fastexpt b (n-1));;