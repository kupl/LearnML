let rec filter p l =
match l with
|[] -> []
|hd::tl -> if (p hd) == true then hd::filter p tl else filter p tl
