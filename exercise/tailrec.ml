(* All functions must be implemented as tail-recursive *)
let l = [ 1; 2; 3; 4; 5 ]

(* List.rev *)
let rev_tr l =
  let rec impl l acc =
    match l with
    | [] -> acc
    | x :: xs -> impl xs (x :: acc)
  in
  impl l []
;;

let l_rev = rev_tr l

(* List.map *)
let map_tr f l =
  let rec impl f l acc =
    match l with
    | [] -> rev_tr acc
    | x :: xs -> impl f xs (f x :: acc)
  in
  impl f l []
;;

let f x = x * x
let l_mapped = map_tr f l

(* List.filter *)
let filter_tr p l =
  let rec impl p l acc =
    match l with
    | [] -> rev_tr acc
    | x :: xs -> if p x then impl p xs (x :: acc) else impl p xs acc
  in
  impl p l []
;;

let p x = x > 2
let l_filtered = filter_tr p l

(* List.partition *)
let partition_tr p l =
  let rec impl p l (acc1, acc2) =
    match l with
    | [] -> rev_tr acc1, rev_tr acc2
    | x :: xs -> if p x then impl p xs (x :: acc1, acc2) else impl p xs (acc1, x :: acc2)
  in
  impl p l ([], [])
;;

(* List.length *)
let length_tr l =
  let rec impl l acc =
    match l with
    | [] -> acc
    | x :: xs -> impl xs (acc + 1)
  in
  impl l 0
;;

let l_length = length_tr l

(* List.nth *)
let nth_tr n l =
  if n < 0
  then failwith "Index out bound!"
  else (
    let rec impl n l acc =
      match l with
      | [] -> failwith "s"
      | x :: xs -> if acc = n then x else impl n xs (acc + 1)
    in
    impl n l 0)
;;

let l_nth = nth_tr 3 l
let cmp a b = if a < b then -1 else if a = b then 0 else 1

(* List.compare *)
let rec compare_tr cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], y :: ys -> -1
  | x :: xs, [] -> 1
  | x :: xs, y :: ys ->
    let res = cmp x y in
    if res = 0 then compare_tr cmp xs ys else res
;;

let l2 = [ 1; 2; 3; 6; 7 ]
let l_compare = compare_tr cmp l l2

(* List.append *)
let append_tr l1 l2 =
  let l1_rev = rev_tr l1 in
  let rec impl l1 acc =
    match l1 with
    | [] -> acc
    | x :: xs -> impl xs (x :: acc)
  in
  impl l1_rev l2
;;

let l_append = append_tr l l2

(* List.concat *)
let concat_tr l =
  let l_rev = rev_tr l in
  let rec impl l acc =
    match l with
    | [] -> acc
    | x :: xs -> impl xs (append_tr x acc)
  in
  impl l_rev []
;;

let ll = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
let l_concat = concat_tr ll

(* List.flatten *)
let flatten_tr = concat_tr

(* List.hd *)
let hd_tr l =
  match l with
  | [] -> failwith "List is empty!"
  | x :: xs -> x
;;

(* List.assoc *)
let rec assoc_tr k l =
  match l with
  | [] -> failwith "Value with key Not Found"
  | (key, v) :: xs -> if key = k then v else assoc_tr k xs
;;

let lk = [ 1, 2; 2, 3; 3, 4 ]
let l_assoc = assoc_tr 2 lk

(* List.split *)
let split_tr l =
  let rec impl l acc =
    match l with
    | [] -> rev_tr (fst acc), rev_tr (snd acc)
    | (a, b) :: xs -> impl xs (a :: fst acc, b :: snd acc)
  in
  impl l ([], [])
;;

let ls = [ 1, 2; 3, 4; 5, 6 ]
let l_split = split_tr ls

(* List.combine *)
let combine_tr l1 l2 =
  let rec impl l1 l2 acc =
    match l1, l2 with
    | [], [] -> rev_tr acc
    | x :: xs, y :: ys -> impl xs ys ((x, y) :: acc)
    | _ -> failwith "Invalid argument: 2 lists do not have the same length"
  in
  impl l1 l2 []
;;

let lc1 = [ 1; 2; 3 ]
let lc2 = [ 4; 5; 6 ]
let l_combine = combine_tr lc1 lc2

(* List.init *)
let init_tr len f =
  let rec impl index f acc =
    if index >= 0 then impl (index - 1) f (f index :: acc) else acc
  in
  impl (len - 1) f []
;;

let f x = x + 1
let l_init = init_tr 4 f

(* List.mem *)
let rec mem_tr e l =
  match l with
  | [] -> false
  | x :: xs -> if x = e then true else mem_tr e xs
;;

let e = 2
let e2 = 10
let l_mem = mem_tr e l
let l_mem2 = mem_tr e2 l

(* List.filter_map *)
let filter_map_tr f l =
  let rec impl f l acc =
    match l with
    | [] -> rev_tr acc
    | None :: xs -> impl f xs acc
    | Some x :: xs -> impl f xs (f x :: acc)
  in
  impl f l []
;;

(* List.find *)
let rec find_tr p l =
  match l with
  | [] -> failwith "Not found!"
  | x :: xs -> if p x then x else find_tr p xs
;;

(* List.find_all *)
let find_all_tr = filter_tr

(* List.exists *)
let rec exist_tr p l =
  match l with
  | [] -> false
  | x :: xs -> if p x then true else exist_tr p xs
;;

(* List.map2 *)
let map2_tr f l1 l2 =
  let rec impl f l1 l2 acc =
    match l1, l2 with
    | [], [] -> rev_tr acc
    | x :: xs, y :: ys -> impl f xs ys (f x y :: acc)
    | _ -> failwith "2 Lists must have same length!"
  in
  impl f l1 l2 []
;;

let f x y = x + y
let l_map2 = map2_tr f l l2
let l_exist = exist_tr p l
let p x = x = 2
let l_find = find_tr p l
let lfm = [ None; Some 1; None; Some 2; None; None; Some 3 ]
let l_filter_map = filter_map_tr f lfm
