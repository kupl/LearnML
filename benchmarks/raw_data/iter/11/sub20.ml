let rec iter(n,f) =
    if n<0 then raise (Invalid_argument "Iterator")
    else if n=0 then function x -> x
    else function x -> iter(n-1,f) (f x)  
(*
let test = iter(10, function x -> 2+x) 0
let _ =
print_int test;
print_newline() *)
