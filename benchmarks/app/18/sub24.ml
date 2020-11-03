let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> [] (* TODO *);;
let rec equalcheck = fun n lst ->match lst with
|[ ]->false |hd ::tl ->(hd==n) || (equalcheck n tl);;
let rec app : 'a list -> 'a list ->'a list
= fun a1 a2 -> match a1 with
|[]-> a2
|hd::tl ->if equalcheck hd a2 then app tl a2 else app tl (a2@hd::[]);;
app [4;5;6;7] [1;2;3;4];;
        