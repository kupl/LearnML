let rec dividor : int*int -> bool
= fun (n, c) -> if c == 1 then true 
  else if n mod c == 0 then false else dividor (n, c-1);;

let prime : int -> bool
= fun n -> dividor (n,n-1);;
  
prime 4;;
prime 17;;
prime 29;;