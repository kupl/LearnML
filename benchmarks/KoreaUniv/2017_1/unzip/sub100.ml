(* problem 7*)

let rec lappend l n =
  match l with
  | [] -> n::[]
  | hd::tl -> hd::(lappend tl n)

let rec unzip2 l out1 out2 =
  match l with
  | [] -> (out1, out2)
  | (f, s)::tl -> unzip2 tl (lappend out1 f) (lappend out2 s)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip2 lst [] []
