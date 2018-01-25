(* problem 7 *)
let rec decompose_x (x, _) = x::[]
let rec decompose_y (_, y) = y::[]
let rec decompose_xst = fun l -> match l with
        | [] -> []
        | hd::tl -> (decompose_x hd)@(decompose_xst tl)
let rec decompose_yst = fun l -> match l with 
        | [] -> []
        | hd::tl -> (decompose_y hd)@(decompose_yst tl)
let rec unzip 
= fun lst -> (decompose_xst lst, decompose_yst lst) 