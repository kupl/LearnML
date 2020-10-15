(* let c=1
   let d=10
   let incr = fun x -> x+1 *)

let rec sigma f a b = 
    if a>b then 0
    else if a=b then (f a)
    else (f a) + sigma f (a+1) b

(* let test = sigma(c,d, incr)

let _ =
print_int test;
print_newline() *)
