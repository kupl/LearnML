let rec iter : (int * ('a -> 'a) -> 'a -> 'a) = function (n, f) -> (
  let rec iter2 n f r = if (n <= 0) then r else (
    let twice x = f (f x) in (
      if (n mod 2 == 1) then (
        iter2 (n / 2) twice (function x -> f(r x))
      ) else (iter2 (n / 2) twice r)
    )
  ) in (iter2 n f (function x -> x))
)

