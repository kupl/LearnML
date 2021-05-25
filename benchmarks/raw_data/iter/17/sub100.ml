let rec iter ((num : int), (fnt : ('a -> 'a))) : 'a -> 'a =
  if (num == 0) then 
    fun x -> x
  else
    let comp (f : 'a -> 'a) (g: 'a -> 'a) (x : 'a) = f(g(x)) in
    comp (fnt) (iter(num-1, fnt))
    