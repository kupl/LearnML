(* let rec print_list myList = match myList with
  | [] -> print_endline " "
  | head::body ->
    (print_int head;
      print_string " ";
      print_list body) *)

let rec remove_duplicates list3 = match list3 with
  | [] -> []
  | head::tail -> head::(remove_duplicates (List.filter (fun x -> x!=head) tail))

let merge (list1,list2) =
  let concatenated = List.append list1 list2 in
  let filtered_list = remove_duplicates concatenated in
  let sorted = List.sort compare filtered_list in
  List.rev sorted
(*
let final = merge([0;1;2;3;4;5;10;2;4;55], [6;29;24;5;2;3;1])
let _ = print_list final *)
