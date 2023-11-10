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

let rec fac n =
   if n = 0 then 1
   else n * fac (n - 1)

let rec fac_tr n =
  let rec helper acc n = 
    if n == 1 
    then acc (* return accumulator *)
    else helper (n * acc) (n - 1) in
  helper 1 n
(* ======================================================================== *)

(* function is always a shorthand for "fun arg -> match arg with" *)
let rec remove a = function
  | [] -> []
  | x :: xs -> if x = a then remove a xs else x :: remove a xs

let rec remove_tr a lst = 
  let rec helper acc a lst = match lst with
  | [] -> List.rev acc (* rev if the order should be kept: List.rev is tail recursive *)
  | x::xs -> if x = a then helper acc a xs else helper (x::acc) a xs
in helper [] a lst

let lst = [1; 1; 2; 3; 1; 4; 5; 6]
let lst = remove_tr 1 lst
(* ======================================================================== *)


(* all elements satisfy p is partitioned into a(the first list), otherwise b(the second list) *)
let rec partition p l = match l with
     | [] -> [],[]
     | x::xs ->
       let a,b = partition p xs in
       if p x then x::a,b else a,x::b


let rec partition_tr p l =
  (* could also pass the the 2 parts of acc seperately
     e.g helper left right p l *)
  let rec helper acc p l =
    let (yes, no) = acc in  
    match l with
    | [] -> (List.rev yes, List.rev no) (* use List.rev if the order need to be kept*)
    | x::xs -> if p x 
      then helper ((x::yes),no) p xs 
      else helper (yes,(x::no)) p xs
    in helper ([],[]) p l 

  let p a = a > 2
  let lst = [1; 2; 3; 4; 5]
  let part = partition_tr p lst

  (* todo: tailrec in exams *)