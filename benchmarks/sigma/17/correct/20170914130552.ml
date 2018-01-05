let sigma (a,b,f) =
  let rec sigma_rec (n, ret) =
    if (n<=b) 
    then sigma_rec ((n+1),(ret+ (f n)))
    else ret
  in
  (sigma_rec (a,0))

(*
let raw x = x

let _ = print_int (sigma (1,10,raw))
*)
