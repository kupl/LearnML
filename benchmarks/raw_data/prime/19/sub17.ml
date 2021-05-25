let rec prime : int -> bool
= fun n -> (*TODO*)
let rec check n k = 
  if k = 1 then true else
    if n mod k = 0 then false else check n (k - 1) in
  check n (n - 1);;