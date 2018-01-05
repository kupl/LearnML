let merge (left, right) : int list=
  let negCompare a b = ~- (compare a b) in
  List.merge negCompare left right
