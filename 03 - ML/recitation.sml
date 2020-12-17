Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* Lambda functions *)
fun compose f g = fn x => f (g x)

(* Pattern matching *)
fun fib n = case n of
    0 => 0
  | 1 => 1
  | n => fib (n - 1) + fib (n - 2)

fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

val rec fib = fn 0 => 0 | 1 => 1 | n => fib (n - 1) + fib (n - 2)

(* Albebraic Data Types *)
datatype day = Mon | Tue | Wed | Thr | Fri | Sat | Sun

datatype maybe_int = no_int | an_int of int

fun safe_division x y =
  if y = 0
  then no_int
  else an_int (x div y)

datatype tree = empty
              | leaf of int
              | node of tree * tree (* takes a 2-tuple of trees as argument *)

val t = empty
val t = leaf 42
val t = node (empty, empty)
val t = node (leaf 15, node (empty, empty))

fun max x y = if x > y then x else y

(* Pattern matching on ADTs *)
fun depth empty = 0
  | depth (leaf x) = 1
  | depth (node (left, right)) =
    let
      val left_depth = depth left
      val right_depth = depth right
      val max_child_depth = max left_depth right_depth
    in
      1 + max_child_depth
    end

(* Pattern matching on lists and tuples *)
fun second (a, b, c) = b

fun length [] = 0
  | length (x::xs) = 1 + length xs

(* Polymorphism *)
datatype 'a tree = empty
                 | leaf of 'a
                 | node of 'a tree * 'a tree


(* Mutually recursive functions, data types *)
fun odd 0 = false
  | odd 1 = true
  | odd n = even (n - 1)
  and
    even 0 = true
  | even 1 = false
  | even n = odd (n - 1)

(* Commented map *)
fun map (f: 'a -> 'b) ([]: 'a list) = []
  | map f (x::xs) = (f x)::(map f xs)

(* Simplified map *)
fun map f [] = []
  | map f (x::xs) = (f x)::(map f xs);

(* Types *)
safe_division;
safe_division 5;
safe_division 5 7;

(* Solving typing equations *)
fun foo f g x y = if (f x) then (g x) else (g y)

(* Exercise: ADT *)
datatype expr
  = Num of int (* a constant: c *)
  | Var (* a variable: x *)
  | Add of expr * expr (* addition *) (* e1 + e2 *)
  | Mult of expr * expr (* multiplication: *) (* e1 * e2 *)
  | Pow of expr * int (* exponentiation *) (* e1^n *)

(* Excercise: Functions *)
fun derivative (Num _) = Num 0
  | derivative Var = Num 1
  | derivative (Add (e1, e2)) = Add (derivative e1, derivative e2)
  | derivative (Mult (e1, e2)) = Add (Mult (derivative e1, e2), Mult (e1, derivative e2))
  | derivative (Pow (e1, n)) = Mult (Mult (Num n, Pow (e1, n - 1)), derivative e1)

(* Exercise: Type Inference *)
fun h x y z = z x (y x)

(*
   x: 'a
   y: 'a -> 'b
   z: 'a -> 'b -> 'c
   h: 'a -> ('a -> 'b) -> ('a -> 'b -> 'c) -> 'c
*)

(* Type inhabitation *)
(* f: 'a * 'b -> 'a *)
fun f (a, b) = a

(* h: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
(*
   f: 'a -> 'b
   g: 'b -> 'c
*)
fun f f g a = g (f a)

fun foo f g a = [g [f a]]
