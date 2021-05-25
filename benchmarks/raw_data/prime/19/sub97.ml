let prime : int -> bool
= fun n -> 
  match n with
  0 -> false | 1 -> false | _ -> let x = (n-1) in
    let rec modZero x n =
      match x with
        1-> true
        | _ -> match n mod x with
          0 -> false
          | _ -> modZero (x-1) n
        in
        modZero x n
;;

(*prime 7;; true*)
(*prime 4;; false*)