let prime : int -> bool
= fun n -> 
  let rec isPrime = 
    fun x y -> if x=1 then false else
      match y with
        | 1 -> true
        | _ -> if x mod y = 0 then false else isPrime x (y-1) in
  isPrime n (n-1);;

prime 2;;
prime 3;;
prime 4;;
prime 17;;