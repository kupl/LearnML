(* PL HW2-1, "이쁘개"
   2007-11738
   알렉산더*)

(* Type defenition *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
         | Poland | Portugal | Italy | Germany | Sweden | England
         | Croatia | Argentina | A

type tourna = LEAF of team
            | NODE of tourna * tourna

(* type of printable treee *)
type printTree = TLEAF of level
               | TNODE of printTree * printTree * level
and level = int


(*
(* pptree: tourna -> unit *)a
let pptree t = 
*)
    (* power function x in n power; powerInt: int*int -> int *)
    let rec powerInt (x, n) =
        if n = 0 then 1
        else x * (powerInt (x, (n-1))

    
    (* find the maximum depth of the tree; depth: tourna*int -> int *)
    let depth tour =
        let rec depthInner (t, max) =
            let maxInt(int1, int2) =
                if int1 < int2 then int2 else int1
            in
            match t with
                LEAF l -> max
              | NODE (t1, t2) -> maxInt(depthInner(t1, max+1), depthInner(t2, max+1))
        in
        depthInner (tour, 0)

    (* transform tourna to printTree *)
    let tour2tree tour =
        let rec inner (tour, lvl) =
            match tour with
              LEAF t -> TLEAF lvl
            | NODE (t1, t2) -> TNODE (inner (t1, lvl-1), inner (t2, lvl-1), lvl)
        in
        (* tour2tree main *)
        inner (tour, depth tour)



    (* make list of node string, like |-----|; str_node: int -> string list *)
    let str_node floor =
        let num = 
            powerInt (2, floor) - 1 
        in
        let rec line_repeat (strList, times) =
            if times = 0 then strList
            else line_repeat(("-"::strList), times-1) 
        in
        (* str_node main*)
        if num = 0 then ["|"]
        else "|"::(line_repeat (["|"], num))
    
    (* make list of space string; str_space: int -> string list *)
    let str_space floor =
        let num = (powerInt(2, floor)-1)/2 in
        let rec space_repeat (strList, times) =
            if times = 0 then strList
            else space_repeat((" "::strList), times-1)
        in
        space_repeat ([], num)
    
    (* make list of NODE string BLOCK, like " |---| "; str_node_block; int -> string list *)
    let str_node_block floor =
        (str_space floor) @ (str_node floor) @ (str_space floor)

    (* make list of LEAF string BLOCK, like "   |   "; str_leaf: int -> string list *)
    let str_leaf_block floor =
        [" "] @ (str_space floor) @ ["|"] @ (str_space floor) @ [" "]
    



    (* get list of leafs (buttom line); getButtom: printTree -> string list *) 
    (* start from 1th level from buttom *)
    let rec getButtom (t, level) =
        match tour with
            TLEAF lvl -> (if (lvl > level) then (str_leaf_block lvl)
                         else (str_leaf_block level)
                         )
          | NODE (t1, t2, lvl) ->( if (lvl > level) then (getButtom (t1, level))@[" "]@(getButtom(t2, level))
                                  else (str_node_block level)
                                 )

    let compose t =
        let rec comInner (t, times) =
            if times = 



    (* Print string list; toPrint: string list -> unit *)
    let toPrint str_list =
       print_string (String.concat "" str_list)
    in
    

(**********************************************)
    (* pptree main *)
    toPrint (toStringList t)

(* end pptree *)


(* for test *)
(* test tourna *)
let t11 = LEAF A
let t1 = NODE (LEAF A, LEAF A)
let t2 = NODE (t1, t11)
let t3 = NODE (t2, t2)
let t4 = NODE (t3, t3)
let t5 = NODE (t4, t4)

