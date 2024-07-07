(* Write tail recursive versions of the following functions (without changing their types) *)

(* For a function f to be tail recursive, ALL recursive calls within should be tail call, which
   means no further computation with results of any recursive call!

   if f and g call each other, and g is not tail recursive then f is also not even it seems to be at the first glance.
     Because f calls g, and g has non-tail calls, which means f also has non-tail calls.

   In addition, all functions (even non-recursive) used within need to be tail recursive
   according to specific problem statements. *)

(* The general idea behind tail-recursive computation: instead of waiting for the nested call to complete
   and then adding something to the output, we do the adding first and pass what has been "added so far"
   into the recursive call. ==> helper function with additional argument: accumulator (what has been adding so far)
   and we return the accumulatior in the end, cuz its what has been added so far, namely our final result *)

(* ======================================================================================== *)
(* Simple rewritings: tutorial 8 *)
let rec fac n = if n = 0 then 1 else n * fac (n - 1)

let rec fac_tr n =
  let rec helper acc n =
    if n = 1 then acc (* return accumulator *) else helper (n * acc) (n - 1)
  in
  helper 1 n
(* ======================================================================================== *)

(* "function" keyword is always a shorthand for "fun arg -> match arg with" *)
let rec remove a = function
  | [] -> []
  | x :: xs -> if x = a then remove a xs else x :: remove a xs

(* Question: How to implement without module List? --> implement a tr rev myself *)
let rec remove_tr a lst =
  let rec helper acc a lst =
    match lst with
    | [] ->
        List.rev
          acc (* rev if the order should be kept: List.rev is tail recursive *)
    | x :: xs -> if x = a then helper acc a xs else helper (x :: acc) a xs
  in
  helper [] a lst

let lst = [ 1; 1; 2; 3; 1; 4; 5; 6 ]
let lst = remove_tr 1 lst
(* ======================================================================================== *)

(* all elements satisfy p is partitioned into a(the first list), otherwise b(the second list) *)
let rec partition p l =
  match l with
  | [] -> ([], [])
  | x :: xs ->
      let a, b = partition p xs in
      if p x then (x :: a, b) else (a, x :: b)

(* Question: How to implement without List.rev? --> implement a tr rev myself *)
let rec partition_tr p l =
  (* could also pass the the 2 parts of acc seperately
     e.g helper left right p l *)
  let rec helper acc p l =
    let yes, no = acc in
    match l with
    | [] ->
        (List.rev yes, List.rev no)
        (* use List.rev if the order need to be kept*)
    | x :: xs ->
        if p x then helper (x :: yes, no) p xs else helper (yes, x :: no) p xs
  in
  helper ([], []) p l

let p a = a > 2
let lst = [ 1; 2; 3; 4; 5 ]
let part = partition_tr p lst

(* ======================================================================================== *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(* a tail recursive implemenation of List.rev *)
let rec rev_tr l =
  let rec impl l stack =
    match l with [] -> stack | x :: xs -> impl xs (x :: stack)
  in
  impl l []

(* a tail recursive implementation of l1@l2 *)
let append_tr l1 l2 =
  let l1_reved = rev l1 in
  let rec impl l1 stack =
    match l1 with [] -> stack | x :: xs -> impl xs (x :: stack)
  in
  impl l1_reved l2

(* a tail recursive implementaion of inorder traversal of a binary search tree *)
let inorder_list tree =
  (* stack: stores the Nodes we traversed so far *)
  (* acc: stores the final inorder list result *)
  let rec impl tree stack acc =
    match tree with
    | Empty -> (
        match stack with
        | [] -> rev acc
        | n :: ns -> (
            match n with
            | Empty -> failwith "Empty should not be pushed onto the stack"
            | Node (x, l, r) -> impl r ns (x :: acc)))
    | Node (v, l, r) -> impl l (Node (v, l, r) :: stack) acc
  in
  impl tree [] []

(* ======================================================================================== *)
(* ======================================================================================== *)
(* Endterm SS23 *)
(* Multiple choice: which is the following functions is tail-recursive according to the definition in the lecture? *)
(* non-recursive *)
let rec h a xss ys =
  let rec rev a = function [] -> a | x :: xs -> rev (x :: a) xs in
  let rec help y = function
    | [] -> [ y ]
    | x :: xs ->
        let tail = help y xs in
        x :: tail
  in
  match (xss, ys) with
  | xs :: xsst, y :: yst ->
      h (help y xs :: a) xsst yst (* <-- here the result of help is used by h*)
  | _ -> rev a

(* tail recursive *)
let g ys xss =
  let rec help1 a xss = function
    | [] -> help2 a xss
    | y :: ys' -> help1 (y :: a) xss ys'
  and help2 a = function
    | (x :: xs) :: xss -> help2 (x :: a) (xs :: xss)
    | [] :: xss -> help1 a xss ys
    | [] -> a
  in
  help2 [] xss

(* non-tail recursive*)
let rec a cc bs cs ds =
  let snd (_, x) = x in
  let help x = x + 1 in
  match (bs, cs, ds) with
  | b :: bs, c :: cs, d :: ds ->
      if b < c then a (b :: cc) bs (c :: cs) ds
      else if c < b then a (c :: cc) (b :: bs) cs ds
      else (help d, snd (a cc bs cs ds))
        (* <-- here the result of help is not directly returned, but used to form a tuple *)
  | _ -> (0, 1)

(* non-tail recursive *)
let rec f = function
  | [] -> Some []
  | [ x ] -> Some [ x ]
  | x :: y :: xs -> (
      match f (y :: xs) with
      (* <-- the result of f is used for pattern-matching*)
      | Some xs' ->
          if x < y then Some (x :: xs') else if x < y then Some xs' else None
      | None -> None)

(* ======================================================================================== *)
(* Retake SS23 *)

(* Multiple choice: which is the following is tail recursive? *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(* non-tail recursive *)
let rec size acc = function
  | Leaf -> acc
  | Node (l, x, r) ->
      size
        (size (acc + 1) l)
        r (* <--- here the result of size is reused for another call of size*)

(* non-tail recursive *)
let rec to_list acc = function
  | Leaf -> List.rev acc
  | Node (l, x, r) ->
      let xs = to_list acc l in
      to_list (x :: xs) r
(* <--- here the result of to_list is reused for another call of to_list*)

(* non-tail recursive *)
let rec find_along path t =
  match (t, path) with
  | Leaf, _ -> []
  | _, [] -> []
  | Node (l, x, r), b :: xs ->
      if b then x :: find_along xs r
        (* <--- here the result of find_along is reused for element appending to a list*)
      else x :: find_along xs l (* <--- the same as above *)

(* tail recursive *)
let rec insert acc y = function
  | Leaf -> acc
  | Node (l, x, r) ->
      if y < x then
        insert ((true, x, r) :: acc) y l (* <--- immediately returned*)
      else insert ((false, x, l) :: acc) y r (* <--- immediately returned*)

(* ======================================================================================== *)

(* Rewriting: Endterm SS23: *)
(* The function foldr_len takes three arguments: f, z, and xss. The argument xss is a list
   of lists. For each list xs in xss, foldr_len produces a pair: the first element in the
   pair is the result of a right fold over xs using f, with z as the initial value. The
   second element is the length of xs. The resulting pairs are returned in the same order as
   the input list.

   Implement foldr_len_tr, an alternative, tail-recursive definition of foldr_len.

   Your implementation of fold_len_tr must:
   For any arguments, return the same result as foldr_len, assuming that foldr_len has enough
   stack space. Use constant stack space (independent of the lengths of xss and any nested
   list in xss).

   You may not use:
   functions from the List, ListLabels, or Seq modules. the OCaml  Tail Modulo Cons
   ([@tail_mod_cons]) feature (this was not covered in the lecture  and if you don't know
   what it is, you won't accidentally use it).
*)
let foldr_len f z xss =
  let rec inner_helper = function
    | [] -> (z, 0)
    | x :: xs ->
        let z', l = inner_helper xs in
        (f x z', l + 1)
  in
  let rec outer_helper = function
    | [] -> []
    | xs :: xss -> inner_helper xs :: outer_helper xss
  in
  outer_helper xss

(* ======================================================================================== *)

(* Rewriting: Retake SS23: *)
(* The function foldrs_i_opt takes three arguments: f, z, and xss. The argument xss is a
   list of lists of 'a option values. For each list xs in xss, the function foldrs_i_opt
   performs a right fold over xs using f, with z as the initial value. Since the elements of
   xs are option values, only the x in Some x are passed to f, and None values are ignored.
   Additionally, the index of each value within xs is passed to f, with the index being
   incremented regardless of whether the value is Some_or None. The values resulting from the
   folds are returned in the same order as the input list. *)

let rec foldrs_i_opt f z =
  let rec foldr_i_opt i = function
    | [] -> z
    | Some x :: xs -> f i x (foldr_i_opt (i + 1) xs)
    | None :: xs -> foldr_i_opt (i + 1) xs
  in
  function [] -> [] | xs :: xss -> foldr_i_opt 0 xs :: foldrs_i_opt f z xss

(* Def of fold_right *)
let rec fold_right f l accu =
  match l with [] -> accu | a :: l -> f a (fold_right f l accu)
(* val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
   fold_right f [a1; ...; an] init is f a1 (f a2 (... (f an init) ...)). Not tail-recursive. *)

(* fold_left *)
let rec fold_left f accu l =
  match l with
  | [] -> accu (* treat the result of (f accu a) as the new accumulater *)
  | a :: l -> fold_left f (f accu a) l

(* equivalence using match *)
(* here foldrs_i_opt is like the fold_right, f is the first arg function *)
let rec foldrs_i_opt f z xss =
  (* foldr_i_opt is essentially an implementation of fold_right *)
  let rec foldr_i_opt i xss =
    match xss with
    | [] ->
        z
        (*f is like the function applied to fold_right, namely
           the first arg, just accepts additional argument index i,
           it is the actual operation on each 'a option of x, just not defined here*)
        (* x is the current elem(list of a' option) to be processed, next is acc *)
    | Some x :: xs -> f i x (foldr_i_opt (i + 1) xs)
    | None :: xs -> foldr_i_opt (i + 1) xs
  in
  match xss with
  | [] -> []
  | xs :: xss -> foldr_i_opt 0 xs :: foldrs_i_opt f z xss

let rec foldrs_i_opt_tr f z xss =
  let rec foldr_i_opt i xss =
    let rec helper acc i xss =
      match xss with
      | [] -> acc
      | Some x :: xs -> helper (f i x acc) (i + 1) xs
      | None :: xs -> helper acc (i + 1) xs
    in
    match xss with [] -> [] | _ -> helper z 0 xss
  in
  foldr_i_opt 0 xss

let lst = [ 1; 2; 3; 4 ]
let suml = List.fold_left (fun acc ele -> acc + ele) 0 lst
let sumr = List.fold_right (fun ele acc -> ele + acc) lst 0

(* solution: *)
module Direct = struct
  (** more direct implementation *)
  let rec rev acc = function [] -> acc | x :: xs -> rev (x :: acc) xs

  let rec len acc = function [] -> acc | _ :: xs -> len (acc + 1) xs

  let foldrs_i_opt f z =
    let rec fold_helper acc i = function
      | [] -> acc
      | Some x :: xs -> fold_helper (f i x acc) (i - 1) xs
      | None :: xs -> fold_helper acc (i - 1) xs
    in
    let rec map_helper acc = function
      | [] -> rev [] acc
      | xs :: xss ->
          map_helper (fold_helper z (len 0 xs - 1) (rev [] xs) :: acc) xss
    in
    map_helper []
end
