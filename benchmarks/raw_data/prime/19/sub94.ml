
let rec prime_test n a : bool = 
  match a with
    |1-> true
    |_->if (n mod a=0) then false else (prime_test n (a-1));;
    

let rec prime : int -> bool
= fun n -> (*TODO*)
  match n with
    |1->false
    |_-> (prime_test n (n-1));;



