(* PL HW2-1, "이쁘개"
   2007-11738
   알렉산더*)

(* Type defenition *)
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
         | Poland | Portugal | Italy | Germany | Sweden | England
         | Croatia | Argentina | A | B

type tourna = LEAF of team
            | NODE of tourna * tourna

(* type of printable treee *)
type printTree = TLEAF of level
               | TNODE of printTree * printTree * level
and level = int


(* pptree: tourna -> unit *)
let pptree t = 
    
    (* power function x in n power; powerInt: int*int -> int *)
    let rec powerInt (x, n) =
        if n = 0 then 1
        else x * (powerInt (x, (n-1)))
    in
    
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
    in

    (* transform tourna to printTree *)
    let tour2tree tr =
        let rec inner (tour, lvl) =
            match tour with
              LEAF t -> TLEAF lvl
            | NODE (t1, t2) -> TNODE (inner (t1, lvl-1), inner (t2, lvl-1), lvl)
        in
        (* tour2tree main *)
        inner (tr, depth tr)
    in

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
    in

    (* make list of space string; str_space: int -> string list *)
    let str_space floor =
        let num = (powerInt(2, floor)-1)/2 in
        let rec space_repeat (strList, times) =
            if times = 0 then strList
            else space_repeat((" "::strList), times-1)
        in
        space_repeat ([], num)
    in

    (* make list of NODE string BLOCK, like " |---| "; str_node_block; int -> string list *)
    let str_node_block floor =
        (str_space floor) @ (str_node floor) @ (str_space floor)
    in

    (* make list of LEAF string BLOCK, like "   |   "; str_leaf: int -> string list *)
    let str_leaf_block floor =
        [" "] @ (str_space floor) @ ["|"] @ (str_space floor) @ [" "]
    in

    (* make list of ROOT; str_root: int -> string list *)
    let str_root d =
        (str_space (d+1)) @ ["|"] @ (str_space (d+1))
    in
    
    (* make list of spaces; str_spaces: int -> string list 
    let rec str_spaces i =
        if i = 0 then [] 
        else [" "] @ (str_spaces (i-1))
    in
    *)

    (* get list of leafs (buttom line); getButtom: printTree -> string list *) 
    let rec getButtom (t, level) =
        match t with
            TLEAF lvl -> str_leaf_block lvl
                         
          | TNODE (t1, t2, lvl) ->( if (lvl > level) then (getButtom (t1, level))@[" "]@(getButtom(t2, level))
                                  else (str_node_block level)
                                 )
    in

    (* compose: printTree*int -> string list *)
    let compose (t, dep) =
        let rec comInner (t, times) =
            if times <=0 then []
            else getButtom(t, times) @ ["\n"] @ comInner (t, times-1)
        in
        (* main *)
        (str_root dep) @ ["\n"] @ comInner (t, dep)
    in

    (* Print string list; toPrint: string list -> unit *)
    let toPrint str_list =
       print_string (String.concat "" str_list)
    in

 (******  pptree main *******)
    toPrint (compose ((tour2tree t), depth t))

(* end pptree *********************************************************)


(* for test *)
(* test tourna *)
let t11 = LEAF A
let t1 = NODE (LEAF A, LEAF A)
let t2 = NODE (t1, t11)
let t3 = NODE (t2, t2)
let t4 = NODE (t3, t3)
let t5 = NODE (t4, t4)

let a = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (LEAF A, LEAF B))
let b = NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B))
let c = NODE(LEAF A, NODE(NODE(NODE(NODE(LEAF A, LEAF B), NODE(LEAF A, LEAF B)), LEAF B), NODE (NODE (LEAF A, LEAF B), LEAF B)))
