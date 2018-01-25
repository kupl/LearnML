let rec dfact n =
  match n with
  0 | 1 -> 1
  |_ -> n * dfact (n-2)