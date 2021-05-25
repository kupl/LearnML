let rec cprime a l =
  if a = l then true
  else if l mod a = 0 then false 
  else cprime (a+1) l;;

let prime : int -> bool
= fun n -> cprime 2 n


let prime l = cprime 2 l;;
prime 17;;