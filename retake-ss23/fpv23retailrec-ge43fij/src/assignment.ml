(** implementation from the exercise description *)
let rec foldrs_i_opt f z =
  let rec foldr_i_opt i = function
    | [] -> z
    | Some x :: xs -> f i x (foldr_i_opt (i + 1) xs)
    | None :: xs -> foldr_i_opt (i + 1) xs
  in
  function
  | [] -> []
  | xs :: xss -> foldr_i_opt 0 xs :: foldrs_i_opt f z xss


let foldrs_i_opt_tr f z xss =
  let rec impl z xss =
    match xss with
    | [] -> acc
    | Some x :: xs -> f i x (foldr_i_opt (i + 1) xs)
    | None :: xs -> foldr_i_opt (i + 1) xs



(* EXAMPLES:

Example 1:
foldrs_i_opt_tr (fun i s acc -> "(" ^ string_of_int i ^ s ^ ")" ^ acc) ""
  [[None; Some "x"]; [Some "y"; None; Some "z"]]
=
["(1x)"; "(0y)(2z)"]


Example 2:
foldrs_i_opt_tr (fun i s acc -> (i, s) :: acc) []
  [[Some "a"; Some "b"]; [Some "c"; Some "d"; Some "e"]]
=
[[(0, "a"); (1, "b")]; [(0, "c"); (1, "d"); (2, "e")]]


Example 3:
foldrs_i_opt_tr (fun i s acc -> (i, s) :: acc) []
  [[None; Some "b"]; [Some "c"; None; Some "e"]]
=
[[(1, "b")]; [(0, "c"); (2, "e")]]

*)
