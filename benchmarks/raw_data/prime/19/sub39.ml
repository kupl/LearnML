let prime : int -> bool
= fun n ->  match n with
     0 -> false
   | 1 -> false
   | _ -> let a = (n - 1) in
            let rec ndiva a n =
              if (a > 1) then
                match n mod a with
                   0 -> false
                 | _ -> ndiva (a - 1) n
              else
                true
              in
              ndiva a n
;;


prime 4;;