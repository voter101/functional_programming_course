open Printf;;

(* Task #1 *)
1;;
1.0;;
'c';;
"TestString";;
true;;
();;
(* ./fib_opt  0.06s user 0.00s system 87% cpu 0.070 total *)
(* ocamlrun fib_c  0.35s user 0.01s system 95% cpu 0.374 total *)

(* Task #2 *)

if true then 4 else 5;;  (* int = 4 *)
(* if false then 1 else 3.5;; *)
(* 4.75 + 2.34;; *)
false || "ab">"cd";; (* bool = false *)
if true then ();; (* unit = () *)
(* if false then () else 4;; *)
(* let x = 2 in x^"aa";; *)
let y = "abc" in y^y;; (* string = "abcabc" *)
(fun x -> x.[1]) "abcdef";; (* char = 'b' *)
(fun x -> x) true;; (* bool = true *)
let x = [1;2] in x@x;; (* int list = [1; 2; 1; 2] *)
let rec f f = f+f in f 42;; (* int = 84 *)

(* Task #4 *)
let m = 10;;
let f x = m + x;;
let m = 100;;
f 1;; (* 11 - in the momemnt of f declaration - m has been "_attached_" to the
function *)

(* Task #6 *)
let plus x y = x + y;;
let plus' = fun x y -> x + y;;
let plus'' = fun x -> fun y -> x + y;;
let plus_3 = plus'' 3;;

(* Task #7 *)
fun x -> x;; (* 'a => 'a = <fun> *)
let id_int (x: int) = x;;
let compose f g x = f (g x);;
let rec func a = func a;;

(* Task #8 *)
let composition f g x = f (g x);;
let rec iterate f n =
  if n == 0
    then (fun x -> x)
    else composition f (iterate f (n-1));;
let mult x y = iterate ((+) x) (y - 1) x;;
let (@@) x y = iterate (mult x) (y - 1) x;;

(* Zad 9 *)
let hd s = s 0;;
let tl s = fun x -> s (x+1);;

let add a s = fun x -> (s x) + a;;
let map f s = fun x -> f (s x);;
let map2 f s1 s2 = fun x -> f (s1 x) (s2 x);;
let replace n s a =
  if n <= 0 then raise (Invalid_argument "n must be larget than 0")
  else           fun x -> if x mod n == 0 then a else s x;;
let take n s =
  if n <= 0 then raise (Invalid_argument "n must be larget than 0")
  else           fun x -> s (x * n);;
let rec fold f a s =
  fun x ->
    if x == 0 then f a (s x)
    else           fold f (f a (s x)) s (x - 1);;
let tabulate s ?(a = 0) b =
    if a > b  then raise (Invalid_argument "a has to be smaller then b")
    else
      let s' = iterate tl a s
      in fold (fun xs x -> x :: xs) [] s' (b-a);;
