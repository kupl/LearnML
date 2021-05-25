let rec extraf a b =
  if b=1 then true
  else if (a mod b = 0) then false
  else extraf a (b-1);;

let prime : int -> bool
= fun n ->
  extraf n (n-1);;



prime 2;;
prime 3;;
prime 4;;
prime 17;;