(*		problem 1		*)
let rec max : int list -> int
   = fun lst ->
     match lst with
     | [] -> 0
     | hd::[] -> hd
     | hd::tl ->
         let n = max tl in
           if hd < n then n
           else hd
  