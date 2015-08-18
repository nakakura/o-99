let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;

let rec last_two = function
  | [] -> None
  | [x;y] -> Some (x, y)
  | _ :: t -> last_two t;;

let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k-1) t;;

let rec length = function
  | [] -> 0
  | x :: t -> 1 + length t;;

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;

let is_palindrome list =
  list = rev list;;

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten list =
  let rec aux v = function
    | [] -> v
    | One x :: t -> aux (x :: v) t
    | Many x :: t -> aux (aux v x) x
  in List.rev(aux [] list);;

let compress list =
  let rec aux h = function
    | [] -> h
    | x :: t -> if h = [] || x != List.hd(h) then aux (x :: h) t else aux h t
  in List.rev(aux [] list);;

let pack list =
  let rec aux acc current = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | x :: (y :: _ as t) -> if x = y
      then aux acc (x :: current) t
      else aux ((x :: current) :: acc) [] t
  in List.rev(aux [] [] list);;

let encode list = List.map (fun l -> (List.length l, List.hd l)) (pack list);;

let encode2 list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | x :: (y :: _ as t) -> if x = y
      then aux (count + 1) acc t
      else aux 0 ((count + 1, x) :: acc) t
  in List.rev(aux 0 [] list);;

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode_rle list =
  List.map (fun l -> if (List.length l) = 1
    then One(List.hd l)
    else Many(List.length l, List.hd l)) (pack list);;
