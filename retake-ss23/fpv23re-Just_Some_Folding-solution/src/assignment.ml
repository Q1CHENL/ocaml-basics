module type FoldRsIOpt = sig
  val foldrs_i_opt :
    (int -> 'a -> 'b -> 'b) -> 'b -> 'a option list list -> 'b list
end

(** Each of the following modules represents one way of implementing foldrs_i_opt.
    Apart from the first module (which just mirrors the exericse description),
    any one of these would be an acceptable solution. *)

module ExerciseDescription : FoldRsIOpt = struct
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
end

module WithFoldLeft : FoldRsIOpt = struct
  (** tail-recursive implementation, using an implementation of fold_left *)

  let rec fold_left f z = function
    | [] -> z
    | x :: xs -> fold_left f (f z x) xs

  let rev xs = fold_left (fun ys y -> y :: ys) [] xs

  let length xs = fold_left (fun n _ -> n + 1) 0 xs

  let foldr_i_opt_tr f z xs =
    fold_left
      (fun (a, i) -> function None -> (a, i - 1) | Some x -> (f i x a, i - 1))
      (z, length xs - 1) (rev xs)
    |> fst

  let map f xs = fold_left (fun a x -> f x :: a) [] (rev xs)

  let foldrs_i_opt f z xss = map (foldr_i_opt_tr f z) xss
end


module Direct : FoldRsIOpt = struct
  (** more direct implementation *)

  let rec rev acc = function
    | [] -> acc
    | x :: xs -> rev (x :: acc) xs

  let rec len acc = function
    | [] -> acc
    | _ :: xs -> len (acc + 1) xs

  let foldrs_i_opt f z =
    let rec fold_helper acc i = function
      | [] -> acc
      | Some x :: xs -> fold_helper (f i x acc) (i - 1) xs
      | None :: xs -> fold_helper acc (i - 1) xs
    in
    let rec map_helper acc = function
      | [] -> rev [] acc
      | xs :: xss -> map_helper (fold_helper z (len 0 xs - 1) (rev [] xs) :: acc) xss
    in
    map_helper []

end

let foldrs_i_opt = ExerciseDescription.foldrs_i_opt
let foldrs_i_opt_tr = Direct.foldrs_i_opt
