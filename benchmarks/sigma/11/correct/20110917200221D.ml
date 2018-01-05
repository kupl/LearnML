(* let c=1
   let d=10
   let incr = fun x -> x+1 *)

let rec sigma(a,b,f) = 
    if a>b then 0
    else if a=b then (f a)
    else (f a) + sigma(a+1,b,f)

(* let test = sigma(c,d, incr)

let _ =
print_int test;
print_newline() *)
