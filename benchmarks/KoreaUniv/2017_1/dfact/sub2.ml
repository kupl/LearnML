(*4*)
  let product : (int -> int) -> int -> int -> int
  =fun f a b ->
    let rec prod n=
      if n=1 then 1 else n*prod(n-1)
  in 
  f(prod b)/f(prod a)
  ;;

(*5*)
  let dfact :int -> int
  =fun n ->
    if n mod 2 =0 then product (fun x -> 2*x) 2 n/2
    else product (fun x->2*x-1) 1 n/2 ;;