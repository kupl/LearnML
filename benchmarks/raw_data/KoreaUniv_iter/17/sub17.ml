(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
  let rec impl _n =
    if _n = 0 then (fun x -> x)
    else (fun x -> f ((impl (_n - 1)) x)) in
  impl n;;
