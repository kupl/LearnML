exception Error of string

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina

type tourna = LEAF of team
| NODE of tourna * tourna

let rec get_nth lst num =
match lst with [a] -> (if (num=1) then a
                       else if (num>1) then (List.filter (function (x, y) -> (y="|")) a) 
                       else (raise (Error "index < 1")))
   | a::h -> (if (num > 1) then  (get_nth h (num-1))
              else if (num = 1) then a
              else (raise (Error "index < 1")))
   | _-> (raise (Error "empty list"))

let move_right lst num = List.map (function sub_lst -> (List.map (function (y,z) -> (y+num, z)) sub_lst)) lst 
 
let rec merge lst1 lst2 num = if (num<1) then (raise (Error "minus num"))
                         else if (num=1) then []                         
                         else (merge lst1 lst2 (num-1))@[(get_nth lst1 num)@(get_nth lst2 num)]
 
let first_position lst = 
match lst with a::h -> fst (List.nth a 0)
|_->  (raise (Error "empty list 2"))


let rec first_leaf_pos lst =
match lst with [a] -> fst (List.nth a 0)
|a::h -> (first_leaf_pos h) 
|_->  (raise (Error "empty list 2"))

let rec last_leaf_pos lst = 
match lst with [a] -> fst (List.nth a ((List.length a)-1) )
|a::h -> (last_leaf_pos h) 
|_->  (raise (Error "empty list 2"))


let rec mkbar start num =
if (num<=0) then []
else if (num=1) then [(start,"-")]
else ([(start,"-")]@(mkbar (start+1) (num-1)))
 

let rec pow base power =
if(power < 0) then (raise (Error "cant'be fraction"))
else if (power =0) then 1
else base * (pow base (power-1))


 
let rec ppform tour =
let rec ppform_iter tour =
match tour with (LEAF(a)) -> (1,[[(1,"|")]])
| NODE(tour1, tour2) -> (
let (height1, lst1) = (ppform_iter tour1) in
let (height2, lst2) = (ppform_iter tour2) in
let height = (if (height1>=height2) then height1 else height2) + 1 in
let right_num = ((pow 2 (height-1))  + (first_position lst1) - (first_position lst2)) in
let lst2 = (move_right lst2 right_num) in
let lst = (merge lst1 lst2 (height-1)) in
let start = (first_position lst1) in 
let endd = (first_position lst2) in
let bar = mkbar (start+1) (endd-start-1) in
let lst3 = [(List.nth lst1 0)@bar@(List.nth lst2 0)]@lst in
let lst4 = [[(((first_position lst1)+(((pow 2 (height-1))/2))),"|")]]@lst3 in
(height, lst4)
) in
snd(ppform_iter tour)


let rec print_space nth = if (nth<0) then (raise (Error "print negative times"))
                          else if (nth=0) then ()
                          else if (nth=1) then print_string(" ")                   
                          else (print_string(" ");(print_space (nth-1)))

let rec print_seq lst last_num= 
match lst with [(nth,z)]-> (print_space (nth-last_num-1));(print_string z)
|(nth,z)::h -> (print_space (nth-last_num-1));(print_string z);(print_seq h nth)
|_-> ()

let pptree_iter tour_lst =
List.map (function lst -> ((print_seq lst 0);print_string("\n"))) tour_lst

let pptree tour =
snd(pptree_iter (ppform tour),())