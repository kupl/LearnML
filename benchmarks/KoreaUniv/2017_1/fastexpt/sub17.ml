(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> (*TODO*)
  let rec impl _n = 
    if _n = 0 then 1
    else if _n mod 2 = 1 then b * impl (_n - 1)
    else (fun x -> x * x) (impl (_n / 2)) in
  impl n;;