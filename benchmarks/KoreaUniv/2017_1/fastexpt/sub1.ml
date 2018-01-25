let fastexpt : int -> int -> int
= fun b n -> let gop x = x * x in let rec fastexpta b n =
    if n = 0 then 1
    else
      if n mod 2 = 0 then  gop (fastexpta b (n/2))
      else b * (fastexpta b (n-1)) in fastexpta b n