type t = int list

let to_string : t -> string
= fun lst -> (List.fold_left (fun str elem -> str ^ string_of_int elem ^ ";") "[" lst) ^ "]" 


let shrink : t -> t QCheck.Iter.t 
= fun lst -> QCheck.Shrink.list ~shrink:QCheck.Shrink.int lst


let gen : t QCheck.Gen.t 
= (QCheck.Gen.list (QCheck.Gen.int))