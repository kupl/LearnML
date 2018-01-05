let merge: int list * int list -> int list = fun (a, b) ->
  let rec _merge (_a: int list) (_b: int list) (r: int list): int list =
    match (_a, _b) with
      | [], _ -> (r@_b)
      | _, [] -> (r@_a)
      | ha::ta, hb::tb -> (
        if ha > hb
          then _merge ta _b (r@[ha])
          else _merge _a tb (r@[hb])
      )
  in _merge a b []
