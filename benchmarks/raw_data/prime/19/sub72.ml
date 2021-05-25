let prime : int -> bool
= fun n -> (*TODO*)
match n with
     0 -> false
   | 1 -> false
   | _ -> let a = (n - 1) in
            let rec checkZero a n =
              match a with
                 1 -> true
               | _ -> match n mod a with
                         0 -> false
                       | _ -> checkZero (a - 1) n
              in
              checkZero a n
;;
