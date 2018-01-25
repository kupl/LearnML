let dfact : int->int
  = fun n -> let rec df n = if n < 3 then n else n * df (n-2) in df n