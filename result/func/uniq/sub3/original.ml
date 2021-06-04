let rec checkDuplicate a (lst : 'a list) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if a = hd then true else checkDuplicate a tl


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> (
      let temp = hd in

      match checkDuplicate temp tl with true -> uniq tl | false -> lst )
