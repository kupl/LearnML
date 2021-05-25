let prime : int -> bool
= fun n -> 
  let rec prime2 
    = fun (n1, b) ->
      if n1 mod b != 0 then prime2 (n1, (b-1))
      else if n1 mod b = 0 && b != 1 then false
      else true
      in prime2 (n, (n-1));;
      prime 37;;