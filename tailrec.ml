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

let rec fac n = if n = 0 then 1 else n * fac (n - 1)

let rec fac_tr n =
  let rec helper acc n =
    if n == 1 then acc (* return accumulator *) else helper (n * acc) (n - 1)
  in
  helper 1 n
(* ======================================================================== *)

(* function is always a shorthand for "fun arg -> match arg with" *)
let rec remove a = function
  | [] -> []
  | x :: xs -> if x = a then remove a xs else x :: remove a xs

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
(* ======================================================================== *)

(* all elements satisfy p is partitioned into a(the first list), otherwise b(the second list) *)
let rec partition p l =
  match l with
  | [] -> ([], [])
  | x :: xs ->
      let a, b = partition p xs in
      if p x then (x :: a, b) else (a, x :: b)

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

(* ======================================================================== *)
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
  | x :: y :: xs ->
      match f (y :: xs) with (* <-- the result of f is used for pattern-matching*)
      | Some xs' ->
          if x < y then Some (x :: xs') else if x < y then Some xs' else None
      | None -> None

(* ------------------------------------------------------------------------ *)
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
        insert ((true, x, r) :: acc) y l (* <--- immediate returned*)
      else insert ((false, x, l) :: acc) y r
(* <--- immediate returned*)
(* ------------------------------------------------------------------------ *)
