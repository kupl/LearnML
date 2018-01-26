let rec prime : int -> bool
= fun n-> for i = 2 to n-1
            if n mod i = 0 then false
          else then true;;
(* TODO *)
