type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
	    | NODE of tourna * tourna
type ptree = PNODE | PEMPTY | PSEED | PLEAF

exception Invalid_input of string 

let pptree tour = 
 (* pow2 : int -> int  - 2의 제곱승 *)
 let rec pow2 n = 
  if(n=0)
  then 1
  else (2 * (pow2 (n-1)))
 in
 
 (* list_ch : 'a list -> int -> 'a  - int에 해당하는 index에 있는 값은 'a로 바꾼다. *)
 let rec list_ch lst i item = 
  match lst with
   h::t -> (if (i=0)
   	    then (item::t)
	    else (h::(list_ch t (i-1) item))
	   )
   |[] -> raise (Invalid_input "list_ch : out of range")
 in

(*--------------------------------------------------------------------------------------------------------*)

 (* make_ptretty : tourna -> ptree list list  - 출력을 위한 파싱 *)
 let rec make_pretty ugly =
  (* make_PEMPTY_list : int -> ptree list  - int 만큼의 PETMPY로 구성된 ptree list 를 만든다. *)
  let rec make_PEMPTY_list n = 
   if (n=0)
   then []
   else (PEMPTY::(make_PEMPTY_list (n-1)))
  in

  (* make_empty_tail : ptree list list -> int -> ptree list list  - int 만큼 ptree list list 의 뒤에 적절한 PEMPTY만으로 이뤄진 ptree list 를 추가 *)
  let rec make_empty_tail pt n = 
   let pt_size = List.length pt in
   if (n=0)
   then pt
   else (make_empty_tail (pt @ [(make_PEMPTY_list (pow2 pt_size))]) (n-1))
  in

  (* make_balanced : ptree list list * ptree list list -> ptree list list * ptree list list  - 두 ptree의 형식을 갖게 해준다. *)
  let make_balanced (pt1, pt2) = 
   let pt1_size = List.length pt1 in
   let pt2_size = List.length pt2 in
   if (pt1_size = pt2_size)
   then (pt1, pt2)
   else (if (pt1_size < pt2_size)
	 then ((make_empty_tail pt1 (pt2_size - pt1_size)), pt2)
	 else (pt1, (make_empty_tail pt2 (pt1_size - pt2_size)))
	)
  in

  (* merge : ptree list list * ptree list list -> ptree list list  - 두개의 ptree 를 하나로 합쳐준다. *)
  let rec merge (pt1, pt2) =
   match (make_balanced (pt1,pt2)) with
    (h1::t1, h2::t2) -> [h1 @ h2] @ (merge (t1,t2))
    |([],[]) -> []
    |_ -> raise (Invalid_input "Wrong Implementation : need Debuging on merge in make_pretty")
  in

  (*---------------------------------------------------------------------------------------*)
  (* make_pretty 본문 *)
  match ugly with 
   LEAF t -> [[PSEED]]
   |NODE (t1,t2) -> [[PNODE]] @ (merge ((make_pretty t1),
  			               (make_pretty t2)))
 in

(*--------------------------------------------------------------------------------------------------------*)
 (* grow_seed : ptree list list -> ptree list list  - seed의 밑으로 적절한 leaf를 생성해준다. *)
 let grow_seed ptree = 
  (* grow_seed_proc : ptree list list * int list -> ptree list  - grow_seed를 실질적으로 구동하는 함수 *)
  let rec grow_seed_proc (pt,i_list) =
   (* grow_see_row : ptree list * int list -> ptree  - 한 줄 씩 작업을 수행하는 함수 *)
   let rec grow_seed_row (pt_row,i_list_row) =
    match i_list_row with
     h::t -> (grow_seed_row ((list_ch pt_row (h-1) PLEAF),t))
     |[] -> pt_row
   in

   (* update_index_list : ptree list -> int list -> int list  - 적절하게 index list를 update해주는 함수 *)
   let rec update_index_list i_list_up = 
    let f n = (2 * n) in
    
    (* update : int list -> int list  - 다음 단계의 index list 를 만든다. *)
    let update i_list_update = (List.map f i_list_update) in

    (* addition : ptree list -> int -> int list  - PSEED에 의한 새로운 index. *)
    let rec addition pt_add i_add = 
     match pt_add with 
      PSEED::t -> (i_add * 2 - 1)::(addition t (i_add + 1))
      |_::t -> (addition t (i_add + 1))
      |[] -> []
    in
   

    (* update_index_list 시작 *)
    match pt with 
     h::t -> ((update i_list_up)@(addition h 1))
     |[] -> []
   in

   (*----------------------------------------------------*)
   (* grow_seed_proc 본문 *)
   match pt with
    h::t -> (grow_seed_row (h,i_list))::(grow_seed_proc (t,(update_index_list i_list)))
    |[] -> []
   in
  (*---------------------------------------------------------------------------------------*)
  (* grow_seed 본문 *)
  (grow_seed_proc (ptree,[]))
  in
(*--------------------------------------------------------------------------------------------------------*)
 (* print_ptree : string list list -> unit  - 출력을 위한 함수 *)
 let rec print_ptree ptree = 
  (* print_empty : int -> unit  - 입력된 횟수 만큼 공백 출력 *)
  let rec print_empty n = 
  if (n=0)
  then (print_string "")
  else ((print_string " ");
	(print_empty (n-1))
       )
  in

  (* print_branch : int -> unit  - 입력된 횟수 만큼 가지(-) 출력 *)
  let rec print_branch n = 
  if (n=0)
  then (print_string "")
  else ((print_string "-");
 	(print_branch (n-1))
       )	
  in
(*
  (* print_node : unit  - 노드(|) 출력 *)
  let print_node = print_string "|"
  in
*)

  (* print_ptree_proc : ptree list list -> int -> int -> unit  - 실질적으로 print_ptree 를 구동하는 함수 *)
  let rec print_ptree_proc pt height i =
(*   let width = (2 * height - 1) in *)
   let j = (height - i + 1) in
   let branchs = ((pow2 j) - 1) in
   let side_blank = ((pow2 (j-1)) - 1) in
   let middle_blank = ((pow2 j) - 1) in

   (* print_ptree_row : ptree list list -> unit  - ptree를 한 줄씩 출력하는 함수 *)
   let rec print_ptree_row pt_row = 
    (* print_ptree_row_proc : ptree list list -> unit  - print_ptree_row 를 실질적으로 구동하는 함수 *)
    let rec print_ptree_row_proc pt_row_proc = 
     (* print_siblings : 'a -> unit *)
     let print_siblings i = 
     ((print_string "|");
      (print_branch branchs);
      (print_string "|");
      (print_empty middle_blank);
     )
     in

     (* print_leaf : 'a -> unit *)
     let print_leaf_left i = 
      ((print_empty (branchs/2 + 1));
       (print_string "|");
       (print_empty (branchs/2 + 1));
       (print_empty middle_blank);
      )
     in
     
     let print_leaf_right i = 
      ((print_empty (branchs + 2 + middle_blank/2));
       (print_string "|");
       (print_empty (middle_blank/2))
      )
     in

     match pt_row_proc with
      PNODE::PNODE::t -> ((print_siblings 0);
			  (print_ptree_row_proc t)
     			 )
      |PNODE::PSEED::t -> ((print_siblings 0);
			   (print_ptree_row_proc t)
			  )
      |PSEED::PNODE::t -> ((print_siblings 0);
			   (print_ptree_row_proc t)
      			  )
      |PSEED::PSEED::t -> ((print_siblings 0);
      			   (print_ptree_row_proc t)
			  )
      |PEMPTY::PLEAF::t -> ((print_leaf_right 0);
			    (print_ptree_row_proc t)
      			   )
      |PLEAF::PEMPTY::t -> ((print_leaf_left 0);
			    (print_ptree_row_proc t)
      			   )
      |PEMPTY::PEMPTY::t -> ((print_empty (branchs  + 2 + middle_blank));
     			     (print_ptree_row_proc t)
     			    )
      |[PNODE] -> (print_string "|")
      |[PSEED] -> (print_string "|")
      |[] -> (print_string "")
      |_ -> raise (Invalid_input "Wrong Implemtation : need Debuging on print_ptree_row_proc in print_ptree")
    in

    (print_empty side_blank);
    (print_ptree_row_proc pt_row)
   in

   (*---------------------------------------------------------*)
   (* print_ptree_proc 본문 *)
   match pt with
   h::t -> (if (j=1)
   	    then ((print_ptree_row h);
	    	  (print_string "\n")
		 )
	    else ((print_ptree_row h);
		  (print_string "\n");
		  (print_ptree_proc t height (i+1))
		 )
   	   )
   |[] -> raise (Invalid_input "Wrong Implemetation : need Debuging on print_ptree_proc in print_ptree")

  in
  (*---------------------------------------------------------------------------------------*)
  (* print_ptree 본문 *)
  (print_ptree_proc (ptree) (List.length ptree) 1)
 in

 (* 함수 본문 *)
 (print_ptree (grow_seed (make_pretty tour)))
;;

(*
(* test set *)
let a = LEAF Korea 
let b = LEAF Japan 
let c = NODE (a, b) 
let d = NODE (c, a) 
let e = NODE (b, c) 
let f = NODE (c, c) 
let g = NODE (d, a) 
let h = NODE (a, d) 
let i = NODE (a, g) 
let j = NODE (h, d) 
let k = NODE (i, j) 
let l = NODE (c, c) 
let m = NODE (l, l) 
let n = NODE (m, m) 
let o = NODE (n, n) 
let p = NODE (o, o) 


let print_test i = 
 (print_string "a) \n");
 (pptree a);
 (print_string "b) \n");
 (pptree b);
 (print_string "c) \n");
 (pptree c);
 (print_string "d) \n");
 (pptree d);
 (print_string "e) \n");
 (pptree e);
 (print_string "f) \n");
 (pptree f);
 (print_string "g) \n");
 (pptree g);
 (print_string "h) \n");
 (pptree h);
 (print_string "i) \n");
 (pptree i);
 (print_string "j) \n");
 (pptree j);
 (print_string "k) \n");
 (pptree k);
 (print_string "l) \n");
 (pptree l);
 (print_string "m) \n");
 (pptree m);
 (print_string "n) \n");
 (pptree n);
 (print_string "o) \n");
 (pptree o);
 (print_string "p) \n");
 (pptree p)
;;
*)
