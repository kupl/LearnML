
(*problem 4*)
  let rec product =
    fun f a b -> if a=b then f(b)
                 else f(a)*(product f (a+1) b);;
