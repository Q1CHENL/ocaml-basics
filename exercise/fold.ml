let rec fold_left f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs
;;

let rec fold_right f l acc =
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)
;;

let f x y = x + y
let l = [ 1; 2; 3 ]
let xr = fold_right f l 0
let xl = fold_left f 0 l
