let prime : int -> bool
= fun n -> 
 match n with
   | 0 -> false
   | 1 -> false
   | _ -> let m = (n-1) in
          let rec restisZero m n =
            if (m != 0 && m != 1) then
              match n mod m with
              | 0 -> false
              | _ -> restisZero (m-1) n
            else true
            in restisZero m n;;

prime 0;;
prime 1;;
prime 2;;
prime 3;;
prime 4;;