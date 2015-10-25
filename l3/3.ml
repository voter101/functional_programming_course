open List;;

(* Task 1 *)

let solve_rec coefficients x =
  let rec solve_aux coeff current x = match coeff with
    | []    -> current
    | y::[] -> solve_aux [] (current +. y) x
    | y::ys -> solve_aux ys ((current +. y) *. x) x
  in match coefficients with
  | []    -> raise (Invalid_argument "The given list shouldn't be empty")
  | y::ys -> solve_aux ys (x *. y) x;;

let solve_non_rec coefficients x =
  List.fold_left (fun sum e -> (e +. sum *. x)) 0. coefficients;;

(* Task 2 *)

let rec solve_rec2 coefficients x =  match coefficients with
  | []    -> 0.
  | y::ys -> y +. (x *. (solve_rec2 ys x));;

let solve_non_rec2 coefficients x =
  List.fold_right (fun e sum -> sum *. x +. e) coefficients 0.;;
