Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* 1 *)
fun foo f g a = [g [f a]]

(* 2 *)
fun bar mult [] = []
  | bar mult (x::xs) = (x * mult) :: bar mult xs

(* 3 *)
fun part x [] = ([], [])
  | part x (y::ys) =
    let val (a, b) = part x ys
    in if y < x
       then (y::a, b)
       else (a, y::b)
    end

(* 4 *)
fun partSort [] = []
  | partSort [x] = [x]
  | partSort (x::xs) =
    let val (a, b) = part x xs
    in (partSort a) @ (x :: partSort b)
    end

(* 5 *)
fun pSort (op <) L =
    let fun part x [] = ([], [])
          | part x (y::ys) =
            let val (a, b) = part x ys
            in if y < x
               then (y::a, b)
               else (a, y::b)
            end
        fun partSort [] = []
          | partSort (x::xs) =
            let val (a, b) = part x xs
            in (partSort a) @ (x :: partSort b)
            end
    in
      partSort L
    end

(* 6 *)
exception reduce_error

fun reduce f [] = raise reduce_error
  | reduce f [x] = x
  | reduce f (x::xs) = (f x (reduce f xs))

(* 7 *)
datatype 'a tree = leaf of 'a | node of 'a tree list

(* 8 *)
fun fringe (leaf x) = [x]
  | fringe (node xs) = reduce (fn x => fn y => x @ y) (map fringe xs)

(* 9 *)
fun sortTree (op <) (leaf x) = leaf (pSort (op <) x)
  | sortTree (op <) (node xs) = node (map (sortTree (op <)) xs)

(* 10 *)
fun powerSet [] = [[]]
  | powerSet (x::xs) =
    let val subset = powerSet xs
    in
      (map (fn L => x::L) subset) @ subset
    end