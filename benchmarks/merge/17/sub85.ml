let rec merge2 ((a: int list), (b: int list), (answer: int list)) : int list =
     match a with
     |[] -> answer@b
     |h1::t1 ->
          (match b with
          |[] -> answer@a
          |h2::t2 -> if h1 > h2 then (merge2 (t1, b, answer@[h1]))
          else (merge2 (a, t2, answer@[h2])))

let merge ((a: int list), (b: int list)) : int list =
     merge2 (a, b, [])
