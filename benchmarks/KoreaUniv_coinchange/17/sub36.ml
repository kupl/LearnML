  (*problem 8*)
  let rec change : int list -> int -> int
  =fun coins amount -> 
  match coins with 
  [] -> 0
  |hd :: tl -> (fun x -> 
      match x with 
      |0 -> 1 
      |_ -> (if amount > 0 then (if (amount/hd)>0 then ((change coins (amount-hd)) + (change tl amount)) else change tl amount) else 0)) amount


