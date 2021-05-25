let rec length lst =
  match lst with
    []->0
    |hd::tl -> 1+ length tl;;
    
let rec jisu n
= if n < 0 then 1
  else if n = 0 then 1
  else if n = 1 then 10
  else 10 * jisu (n-1);;
  
let rec lst2int : int list -> int
= fun lst ->
  match lst with
    [] -> 0
    |hd::tl -> hd * jisu (length lst-1) + lst2int tl ;;
