let prime : int -> bool
= fun x ->
  match x with
    | 0 -> false
    | 1 -> false
    | _ -> let a = (x - 1) in
            let rec rest a x =
              if (a > 1) then
                match x mod a with
                  | 0 -> false
                  | _ -> rest (a - 1) x
              else
                true
              in
              rest a x
              
              ;;
