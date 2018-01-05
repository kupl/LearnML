let rec merge ((list1 : int list), (list2: int list)) : int list = 
  match (list1, list2) with
  |([], _) -> list2
  |(_, []) -> list1
  |(head1::tail1, head2::tail2) -> 
    if head1 > head2 then head1 :: merge (tail1, list2)
    else head2 :: merge(list1, tail2) ;;




