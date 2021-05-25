let rec prime n= 
match n with
|0 -> false
|1 -> false
|_ -> let x = n/2 in
      let rec determine n x =
      match x with
      |1 -> true
      |_ -> match n mod x with
            |0 -> false
            |_ -> determine n (x-1) in determine n x;;

prime 2;;
prime 3;;
prime 4;;
prime 17;;