module L = CCList
module LA = L.Assoc
module O = CCOption
module P = CCParse
module R = CCResult
module A = CCArray
module F = CCFun
module Pr = Printf

let pr = Pr.printf
let spr = Pr.sprintf

open F

let data_folder = "/home/dalpi/aoc24/ocaml/data/"
let data x = "/home/dalpi/aoc24/ocaml/data/" ^ x

let get_input (p : 'a P.t) (f : string) = P.parse_file p f |> R.get_or_failwith

let read_file (filename : string) : string =
  let fd = open_in filename in
  let s = really_input_string fd (in_channel_length fd) in
  close_in fd;
  s

let is_transitive (l : (int * int) list) =
  let open L in
  fold_left ( && ) true
  @@
  let* x, y = l and* y', z = l in
  return ((not (Int.equal y y')) || List.mem (x, z) l)

let filter_map_i f = snd % L.fold_filter_map_i (fun _ i x -> ((), f i x)) ()

(* filter elements of a list by a function based on the index of elements as well *)
let filter_mapi f l =
  let[@tail_mod_cons] rec aux i = function
    | [] -> []
    | x :: xs -> (
        match f i x with
        | None -> aux (i + 1) xs
        | Some x -> x :: aux (i + 1) xs)
  in
  aux 0 l

let assume (b : bool) : unit option =
  if b then
    Some ()
  else
    None

let rec fold_left_pair (f : 'acc -> 'a -> 'a -> 'acc) (accu : 'acc) = function
  | [] | [ _ ] -> accu
  | x0 :: x1 :: xs -> fold_left_pair f (f accu x0 x1) (x1 :: xs)

let fold_left_w2 = fold_left_pair

let rec fold_left_w3 (f : 'acc -> 'a -> 'a -> 'a -> 'acc) (accu : 'acc) =
  function
  | [] | [ _ ] | [ _; _ ] -> accu
  | x0 :: x1 :: x2 :: xs -> fold_left_w3 f (f accu x0 x1 x2) (x1 :: x2 :: xs)

module Coord = struct
  type t = { x : int; y : int } [@@deriving show]

  let make x y : t = { x; y }
  let v x y : t = { x; y }
  let x (p : t) : int = p.x
  let y (p : t) : int = p.y

  let to_string (c : t) = spr "{%d,%d}" c.x c.y
  let pr (c : t) = pr "%s" (to_string c)

  let compare p1 p2 = CCOrd.(pair int int) (p1.x, p1.y) (p2.x, p2.y)
  let equal p1 p2 = Int.(equal p1.x p2.x && equal p1.y p2.y)
  let hash p = CCHash.(pair int int) (p.x, p.y)

  let ( *. ) a p = v (a * p.x) (a * p.y)
  let ( /. ) p a = v (p.x / a) (p.y / a)
  let ( % ) p m = v (p.x mod m.x) (p.y mod m.y)
  let ( * ) p1 p2 = (p1.x * p2.x) + (p1.y * p2.y)
  let ( + ) p1 p2 = v (p1.x + p2.x) (p1.y + p2.y)
  let ( - ) p1 p2 = v (p1.x - p2.x) (p1.y - p2.y)

  let north = make 0 (-1)
  let east = make 1 0
  let south = -1 *. north
  let west = -1 *. east

  let compass = [| north; east; south; west |]
  let compass0 = [ north; east; south; west ]
  let compass1 = [ (`N, north); (`E, east); (`S, south); (`W, west) ]
  let compass2 = [ `N north; `E east; `S south; `W west ]
end

module P2 = Coord

module ListMatrix = struct
  type 'a t = 'a list list

  let get (board : 'a t) i j : 'a option =
    let open O in
    let* _ = assume (i >= 0 && j >= 0) in
    let* xs = L.nth_opt board j in
    L.nth_opt xs i

  let get' (board : 'a t) (p : P2.t) : 'a option = get board p.x p.y

  let remove_empty_rows (t : 'a list list) : 'a list list =
    L.filter (not % L.is_empty) t

  let foldij f (acc : 'acc) m =
    L.foldi
      (fun acc1 i xs -> L.foldi (fun acc2 j x -> f acc2 i j x) acc1 xs)
      acc m

  let iterij p = foldij (fun _ i j x -> p i j x) ()

  let filter_mapij (f : int -> int -> 'a -> 'b option) =
    foldij (fun acc i j x -> L.cons_maybe (f i j x) acc) []

  let filterij (f : int -> int -> 'a -> bool) =
    filter_mapij (fun i j x -> if f i j x then Some x else None) []

  let mapij (f : int -> int -> 'a -> 'b) = L.mapi (L.mapi % f)

  let map f (m : 'a list) =
    let open L in
    let+ m = m in
    f <$> m

  let size : 'a t -> [ `Square of int | `Rect of int * int | `Hetero ] =
    function
    | [] -> `Square 0
    | x :: xs as l ->
        let nrow = L.length l in
        let ncol = L.length x in
        let others = L.map L.length xs in
        if L.for_all (( = ) ncol) others then
          if nrow = ncol then
            `Square nrow
          else
            `Rect (nrow, ncol)
        else
          `Hetero
end

module ArrayMatrix = struct
  type 'a t = 'a array array

  (* Ideas: array to list functions *)
  let get (board : 'a t) i j : 'a option =
    let open O in
    let* xs = A.get_safe board j in
    A.get_safe xs i

  let ( .-() ) (board : 'a t) (p : P2.t) : 'a option = get board p.x p.y

  let set (board : 'a t) i j x : unit =
    if O.is_some (get board i j) then
      board.(j).(i) <- x
    else
      ()

  let ( .-()<- ) (board : 'a t) (p : P2.t) (a : 'a) : unit = set board p.x p.y a

  let find (p : 'a -> bool) (board : 'a t) =
    let open A in
    find_mapi
      (fun j ts -> find_mapi (fun i x -> if p x then Some (i, j) else None) ts)
      board

  let print ~(print_a : 'a -> unit) = A.iter (print_newline % A.iter print_a)

  let of_list (t : 'a list list) : 'a t = t |> A.of_list %> A.map A.of_list

  let find_mapij f = A.find_mapi (A.find_mapi % fun x y -> f y x)
  let foldij (f : 'b -> int -> int -> 'a -> 'b) =
    A.foldi (fun acc1 j xs -> A.foldi (fun acc2 i x -> f acc2 i j x) acc1 xs)
  let mapij f = A.mapi (A.mapi % fun x y -> f y x)
  let iterij f = A.iteri (A.iteri % fun x y -> f y x)
  let filter_mapij f =
    foldij
      (fun acc i j x -> match f i j x with Some x -> x :: acc | None -> acc)
      []
end

module LM = ListMatrix
module AM = ArrayMatrix

module type BASE_FOLDING = sig
  val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  val foldi : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
end

module type MORE_FOLDING = sig
  val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc
  val foldi2 :
    ('acc -> int -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc
end

module type FOLDING = sig
  include BASE_FOLDING
  include MORE_FOLDING
end

module Make_folding (F : BASE_FOLDING) : FOLDING = struct
  include F

  let fold2 f init a b =
    F.fold (fun acc (a, b) -> f acc a b) init (L.combine_shortest a b)

  let foldi2 f init a b =
    F.foldi (fun acc i (a, b) -> f acc i a b) init (L.combine_shortest a b)
end

let digits_of_int n =
  let rec go acc n = if n > 0 then go ((n mod 10) :: acc) (n / 10) else acc in
  if n = 0 then [ 0 ] else go [] n

let int_of_digits ds =
  let open CCInt in
  let rec go mul = function
    | [] -> 0
    | d :: ds -> (d * (10 ** mul)) + go (mul - 1) ds
  in
  go (L.length ds - 1) ds
