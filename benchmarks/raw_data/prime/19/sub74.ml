let rec primeIter : int -> int -> bool =
  fun i x -> if x < 2 then false else (
    if i * i > x then true else (
      if x mod i = 0 then false else primeIter (i + 2) x
    )
  )

let prime : int -> bool
= fun n -> if n mod 2 = 0 && n != 2 then false else
  (match n with
  | 2 -> true
  | 3 -> true
  | 5 -> true
  | _ -> primeIter 3 n
    )
  