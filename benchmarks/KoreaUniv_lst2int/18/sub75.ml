let rec cal n1 n2 = 
  match n2 with
    0 -> n1
    |_->cal (n1*10) (n2/10);;

let rec lst2int : int list -> int
= fun lst -> match lst with
  []->0
  |h::t->h*(cal 1 (lst2int t)) + lst2int t;;
  
lst2int [1;2234;34;5];;