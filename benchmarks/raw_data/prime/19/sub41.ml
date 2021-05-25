let rec divider n m
          = if (m>1) then 
          match n mod m with
            |0->false
            |_->divider n (m-1)
            else true

            
let prime : int -> bool
= fun n -> 
  match n with
    | 0 -> false
    | 1 -> false
    | n-> let m=(n-1) in divider n m;;
          

prime 2;;
prime 3;;
prime 14;;
prime 17;;
