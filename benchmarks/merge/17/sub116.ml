open List

let merge ((l1:int list), (l2: int list)): int list=
  let rec merge_rev_rec (l1, l2, ret) =
    match l1 with
    |[] -> (append (rev l2) ret)
    |hd::tl -> (
      match l2 with
      |[]-> (append (rev l1) ret)
      |hd2::tl2 -> 
        if (hd>hd2) 
        then merge_rev_rec (tl, l2, (hd::ret)) 
        else merge_rev_rec (l1, tl2, (hd2::ret))
      )
  in rev (merge_rev_rec (l1, l2, []))


let rec print_list l=
  match l with 
  |[]-> (print_endline "")
  |hd::tl-> ((print_int hd); (print_string " "); (print_list tl);)

(*
let _ = print_list [1;2;3;4]  

let _ = print_list ( merge [3;2;1] [6;5;4])

let _= print_list (merge ([], []))
let _= print_list (merge ([1],[]))
let _= print_list(merge ([2;1],[]))
let _ = print_list (merge ([3;2;1], [6;5;4]))
let _ =print_list(merge ([6;5;4], [3;2;1]))

let _ =print_list(merge ([3;2], [6;5;4]))

let _ =print_list(merge ([3;2;1;0], [6;5;4]))

let _ =print_list(merge ([1], [4]))

let _ =print_list(merge ([5;3;1], [6;4;2]))

let _ =print_list(merge ([8;6;4;2], [5;3;1] ))

let _ =print_list(merge ([10;8;7;3;2;1], [9;5;4]))






*)

