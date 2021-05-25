let rec cascadeMod n m =  if n = 2 then true else
                          if m = 1 then true else
                          if n mod m = 0 then false else
                            cascadeMod n (m-1);;

let prime : int -> bool
= fun n -> cascadeMod n (n-1);;

prime 2 = true;;
prime 3 = true;;
prime 4 = false;;
prime 17 = true;;
