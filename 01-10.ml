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
    | [] -> acc
    | x :: t -> if x != List.hd(current) then aux (current :: acc) [x] t
  in aux [] [List.hd(list)] List.tl(list);;
