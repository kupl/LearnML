let prime : int -> bool
= fun n -> 
 let rec divisor x =
   match n with 
     |0->false
     |1->false
     |_->(n mod x!=0 && divisor(x+1) )|| x*x>n
 in divisor 2;;
  
  prime 0;;
  prime 1;; 
  prime 2;;
  prime 3;;
  prime 4;;
  prime 17;;
 