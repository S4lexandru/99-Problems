
(* 
 * The problem statements can be found at: http://https://ocaml.org/learn/tutorials/99problems.html
 * I've omitted the statement + explanations for obvious problems
*)
let rec last = function
   [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs

let rec last_two = function
  | [] -> None
  | _ :: [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: xs -> last_two xs

let rec at n = function
  | [] -> None
  | x :: xs -> if (0 = n) then Some x else at (n - 1) xs

let length list = 
  let rec helper acc = function
    | [] -> acc
    | _ :: xs -> helper (acc + 1) xs
  in
  helper 0 list

let reverse list =
  let rec helper sol = function 
    | [] -> sol
    | x :: xs -> helper (x :: sol) xs
  in
  helper [] list

let rec equals aList bList = match (aList, bList) with
  | ([], []) -> true
  | (a :: aL, b :: bL) -> if a != b then false else equals aL bL
  | _ -> false

let isPalindrom list = equals list (reverse list)

(* remove duplicate entries *)
let compress list = 
  let rec helper acc = function
    | [] -> List.rev acc
    | x :: [] -> List.rev (x :: acc)
    | x :: y :: xs -> if x = y then helper acc (y :: xs) else helper (x :: acc) (y :: xs)
  in
  helper [] list

(* group toghether duplicate entries *)
let pack list =
  let rec helper curr acc = function
    | [] -> acc
    | [x] -> (x :: curr) :: acc
    | x :: (y :: _ as t) ->
      if x = y
      then helper (x :: curr) acc t
      else helper [] ((x :: curr) :: acc) t
  in
  List.rev (helper [] [] list)

let runLengthEncoding list =
  let rec helper ((hd, count) as curr) acc = function 
    | [] -> List.rev (curr :: acc)
    | x :: xs ->
        if x = hd
        then helper (hd, count + 1) acc xs
        else helper (x, 1) (curr :: acc) xs
  in
    match list with 
      | [] -> []
      | hd :: list -> helper (hd, 1) [] list
             

