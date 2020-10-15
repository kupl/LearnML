let sigma f a b =
  let rec _sigma (_a: int) (_b: int) (r: int): int =
    if _a > b
      then r
      else _sigma (_a + 1) _b (r + f _a)
  in _sigma a b 0
