(* CSE/ 2004-11920 / Yeseong Kim/ Prob 1*)

exception Error of string
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let pptree tour =
        let maxLen l1 l2 =
                if ((List.length l1) > (List.length l2)) then (List.length l1)
                else (List.length l2)
        in
        let rec makeString str num =
                if (num = 0) then ""
                else    str^(makeString str (num-1))
        in
        let rec doMerge l1 l2 = (*���� sibling�� ��ġ��*)
                match (l1, l2) with
                        (s1::c1, s2::c2) -> (s1^" "^s2)::(doMerge c1 c2)
                |       ([], []) -> []
                |       (_, _) -> raise (Error "Logic Error doMerge")
        in
        let rec fillTop t l n = (*���̸� ����*)
                if ((List.length l) = n) then l
                else (fillTop t (t::l) n)
        in
        let mergeT t1 t2 l1 l2 = (* �ȸ´� ���̸� ���߾� merge ���� *)
                (doMerge (fillTop t1 l1 (maxLen l1 l2)) (fillTop t2 l2 (maxLen l1 l2)))
        in
        let newTop childTop1 childTop2  = (*�� �Ӹ��� ����*)
                ((makeString " " (String.length childTop1)) ^ "|" ^ (makeString " " (String.length childTop2)))
        in
        let tailToDash str = (*���� �ձ�: ���ڿ��� ������ Dash�� ����*)
                ((String.sub str 0 ((String.index str '|')+1))^(makeString "-" ((String.length str) - (String.index str '|')-1)))
        in
        let headToDash str = (*���� �ձ�: ���ڿ��� �Ӹ��� Dash�� ����*)
                ((makeString "-" (String.index str '|'))^(String.sub str (String.index str '|') ((String.length str)-(String.index str '|'))))
        in
        let subconcatTree l1 l2 = (*�ΰ��� Ʈ���� ��ħ*)
               match (l1, l2) with
                        (top1::c1, top2::c2) -> (newTop top1 top2) :: ((tailToDash top1)^"-"^(headToDash top2)) :: (mergeT top1 top2 c1 c2)
               |        (_, _) -> raise (Error "Logic Error subConcatTree") 
        in
        let rec reindent_L l n = (*�� ������ ��� ������ ���� ������ Tree�� �ø�. �������� ������ ���*)
                match l with
                        h::t -> (h^(makeString " " n))::(reindent_L t n)
               |        [] -> []
        in
        let rec reindent_R l n = (*�� ������ ��� ������ ���� ������ Tree�� �ø�. �������� �������� ���*)
                match l with
                        h::t -> ((makeString " " n)^h)::(reindent_R t n)
               |        [] -> []
        in
        let reindentSize l1 l2 = (*�� ������ ��� ������ �Ұ����� ��� �������*)
                match (l1, l2) with
                        (h1::_, h2::_) -> ((String.length h1) - (String.index h1 '|') - 1) - (String.index h2 '|')
                |       (_, _) -> raise (Error "Logic Error reindentSize") 
        in
        let concatTree l1 l2 = (*���� �ֻ���� ƴ���� �Ȱ��� ���߰�, ����*)
                if ((reindentSize l1 l2) = 0) then (subconcatTree l1 l2)
                else if ((reindentSize l1 l2) > 0) then (subconcatTree l1 (reindent_R l2 (reindentSize l1 l2)))
                else (subconcatTree (reindent_L l1 (-(reindentSize l1 l2))) l2)
        in
        let rec subpptree t =
                match t with 
                        NODE(t1, t2)    -> (concatTree (subpptree t1) (subpptree t2))
                |       LEAF(_)         -> ["|"]
        in
        let rec revPrint strList n =
                if ((List.length strList) = n) then (print_string "")
                else ((revPrint strList (n+1));(print_endline (List.nth strList ((List.length strList)-n-1))))
        in
        (revPrint (subpptree tour) 0)
