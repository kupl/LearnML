let rec merge : (int list * int list) -> int list = fun pair -> 
 match pair with
 | (first, []) -> first
 | ([], second) -> second
 | ((head1::tail1), (head2::tail2)) -> 
  if head1>head2 then head1::merge (tail1, (head2::tail2))
  else head2::merge ((head1::tail1), tail2)