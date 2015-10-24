open List;;

(* Task 2 *)

let rec a1 n = match n with
  | 0 -> 1
  | 1 -> 2
  | _ -> 1 + (2 * a1(n-2)) - a1(n-1);;

let rec a2_aux n i current prev1 =
  if (n == i) then
    current
  else
    a2_aux n (i+1) (2 * prev1 - current + 1) current;;

let a2 n = match n with
  | 0 -> 1
  | 1 -> 2
  | _ -> a2_aux n 1 2 1;;

(*
  * Compare:
  * # a1 1000000;;
  * Stack overflow during evaluation (looping recursion?).
  * # a2 1000000;;
  * - : int = -4099276460824011469
*)

(* Task 3 *)

let rec replace_nth list position_to_replace replace_with =
  match list with
   |    [] -> raise (Invalid_argument "The given list is too short")
   | x::xs ->
      if position_to_replace == 0 then
        replace_with::xs
      else
        x::(replace_nth xs (position_to_replace - 1) replace_with);;

(* Task 4 *)

let rec merge cmp l1 l2 = match (l1, l2) with
  | ([], x) | (x, []) -> x
  |    (x::xs, y::ys) ->
      if cmp x y then
        x :: (merge cmp xs l2)
      else
        y :: (merge cmp l1 ys);;

let rec merge2 cmp l1 l2 =
  let rec merge_aux l1 l2 result = match (l1, l2) with
    |                  ([], []) -> result
    | ([], x::xs) | (x::xs, []) -> merge_aux [] xs (x::result)
    |            (x::xs, y::ys) ->
        if cmp x y then merge_aux xs l2 (x::result)
        else            merge_aux l1 ys (y::result)
  in List.rev (merge_aux l1 l2 []);;

let rec split l =
  let rec split_aux l left right = match l with
    | [] -> (left, right)
    | x :: xs -> split_aux xs right (x::left)
  in split_aux l [] [];;

let rec merge_sort cmp l = match l with
  |  [] -> []
  | [x] -> [x]
  |   l ->
    let left, right = split l
    in merge2 cmp (merge_sort cmp left) (merge_sort cmp right);;

(*
  * Compare:
  * val time_test : float * float =
  *   (0.00522600000000750242, 0.00690799999999569536)
  *
  * merge (>=) (make_test_list 3000000) (make_test_list 3000000);;
  *   Stack overflow during evaluation (looping recursion?).
  * merge2 (>=) (make_test_list 3000000) (make_test_list 3000000);;
  * - : int list = [6000000; 5999999; ...]
*)

let make_test_list n =
  let rec aux m acc = match m with
    | 0 -> List.rev acc
    | m ->
        let i = m + n in  aux (m - 1) (i::acc)
  in aux n [];;

let time_measure f =
  let start = Sys.time() in  f();
  Sys.time() -. start;;

let time_test =
  let l1 = make_test_list 30000 and l2 = make_test_list 30000 in
  (
    time_measure (fun () -> merge  (<=) l1 l2),
    time_measure (fun () -> merge2 (<=) l1 l2)
  );;


(* Task 5 *)

let rec interleave x l = match l with
  |    [] -> [[x]]
  | y::ys -> (x::l) :: List.map (fun a -> y::a) (interleave x ys);;

let rec permutations l = match l with
  | x::xs -> List.concat (List.map (interleave x) (permutations xs))
  |     _ -> [l];;

(* Task 6 *)

let rec suffixes l = match l with
  |    [] -> []
  | x::xs -> (l :: (suffixes xs));;

let rec prefixes l = match l with
  |    [] -> []
  | x::xs -> [x] :: (List.map (fun a -> (x :: a)) (prefixes xs));;
