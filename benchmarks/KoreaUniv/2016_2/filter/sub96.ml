let rec filter pred lst =
  match lst with
  |[] -> []
  |a::[] -> if pred a = true then [a] else []
  |a::b -> filter pred [a] @ filter pred b;;