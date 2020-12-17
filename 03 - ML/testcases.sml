(* 2 *)
val g = bar 30;
g [1,2,3,4,5];
bar 30 [1,2,3,4,5];

(* 3 *)
part 6 [5,2,8,4,1,9,6,10];

(* 4 *)
partSort [];
partSort [87];
partSort [5,2,9,10,12,4,8,1,19];

(* 5 *)
pSort (op <) [1,9,3,6,7];
pSort (op >) [];
pSort (fn(a, b) => length a < length b) [[(3, 3)]];
pSort (fn(a, b) => length a < length b) [[1, 9, 3, 6], [1], [2, 4, 6], [5, 5]];

(* 6 *)
fun g x y = x + y;
reduce g [1,2,3,4,5];
reduce g [3];
reduce (fn x => fn y => x + y) [1, 2, 3, 4, 5];

(* 7 *)
val myTree = node
          [node
             [node [leaf [4, 2, 14], leaf [9, 83, 32], leaf [96, 123, 4]],
              node [leaf [47, 71, 82]], node [leaf [19, 27, 10], leaf [111, 77, 22, 66]], leaf [120, 42, 16]],
              leaf [83, 13]];

(* 8 *)
fringe (node [leaf 1, node [leaf 2, leaf 3], node [node [leaf 4, leaf 5], leaf 6]]);
fringe myTree;

val t2 = node
           [node
              [node [
                 leaf [[5, 5], [1], [2, 4, 6], [1, 9, 3, 6]],
                 leaf [[5, 5], [2, 4, 6], [1]], leaf []]]];
fringe t2;

(* 9 *)
sortTree (op <) myTree;
sortTree (fn(a, b) => length a < length b) t2;

(* 10 *)
powerSet [1,2,3];
powerSet [];

(* 6 put in the end for testing purpose *)
reduce g [];