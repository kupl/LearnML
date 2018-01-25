
(*4*)
  let product : (int -> int) -> int -> int -> int
  =fun f a b ->
    let rec prod n=
      if n=1 then 1 else n*prod(n-1)
  in 
  f(prod b)/f(prod a)
  ;;
