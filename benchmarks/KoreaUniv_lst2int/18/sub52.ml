let rec length lst =
  match lst with 
    | [] -> 0
    | hd::tl -> 1 + length tl;;
      
let rec mul_ten n =
  match n with 
    | 0 -> 1
    |_ -> 10 * mul_ten(n - 1);;
      
let rec lst2int : int list -> int
= fun lst -> (*TODO*)
  match lst with
    | [] -> 0
    | hd::tl -> hd * (mul_ten(length lst - 1)) + lst2int tl;;
    
