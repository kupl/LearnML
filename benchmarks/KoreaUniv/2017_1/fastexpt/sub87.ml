let rec fastexpt b n =
  match n with
  |0 -> 1
  |_ -> match (n mod 2) with
    |0 -> (fastexpt b (n/2))*(fastexpt b (n/2))
    |1 -> (fastexpt b (n-1)) * b
