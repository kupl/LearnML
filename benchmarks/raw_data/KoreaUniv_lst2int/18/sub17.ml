let rec lst2int : int list -> int
= fun lst -> 
  let rec lengthOflist lst =
    match lst with
      [] -> 0
      |(h::t) -> 1 + (lengthOflist t) 
  in
  
  let rec powerOften n =
    match n with
      0 -> 1
      |_ -> 10 * (powerOften (n-1)) 
  in
  
  match lst with
    [] -> 0
    |(h::t) -> (h * (powerOften ((lengthOflist lst) - 1)) + lst2int t);;
    

lst2int [];;
lst2int [2;3;4;5];;
lst2int [3;4;5;6;7;8;9];;

