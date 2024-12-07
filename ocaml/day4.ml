module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult

type dir_x = L | C | R
type dir_y = T | C | B
type dir = { dx : dir_x; dy : dir_y }

type coord = { x : int; y : int }

type solution = dir * coord

(* If centered on a 'X', look for 'M' on adjacent, fix the direction and look for 'A' and 'S' in that direction *)

let mk_coord x y = { x; y }
let mk_dir dx dy = { dx; dy }

let dirs =
  [
    mk_dir C T;
    mk_dir C B;
    mk_dir L C;
    mk_dir R C;
    mk_dir L T;
    mk_dir R T;
    mk_dir L B;
    mk_dir R B;
  ]

let coord_of_dir d =
  mk_coord
    (match d.dx with L -> -1 | C -> 0 | R -> 1)
    (match d.dy with T -> -1 | C -> 0 | B -> 1)

let mk_solution dir pos = (dir, pos)

let assume (b : bool) : unit option =
  if b then
    Some ()
  else
    None

let lookup (p : coord) (board : 'a list list) : 'a option =
  let open O in
  let* _ = assume (p.x >= 0 && p.y >= 0) in
  let* xs = L.nth_opt board p.y in
  L.nth_opt xs p.x

let match_xmas (d : dir) (p : coord) (board : char list list) =
  let d = coord_of_dir d in
  let open O in
  let open CCChar in
  let* x = lookup (mk_coord (p.x + (d.x * 0)) (p.y + (d.y * 0))) board in
  print_char x;
  let* _ = assume (x = 'X') in
  let* x = lookup (mk_coord (p.x + (d.x * 1)) (p.y + (d.y * 1))) board in
  print_char x;
  let* _ = assume (x = 'M') in
  let* x = lookup (mk_coord (p.x + (d.x * 2)) (p.y + (d.y * 2))) board in
  print_char x;
  let* _ = assume (x = 'A') in
  let* x = lookup (mk_coord (p.x + (d.x * 3)) (p.y + (d.y * 3))) board in
  print_char x;
  let* _ = assume (x = 'S') in
  print_newline ();
  return (mk_solution d p)

let p = P.(each_line (many any_char))

let parse_board input =
  P.parse_file p input |> Result.get_ok |> L.filter (fun l -> l <> [])

let search (b : char list list) =
  let open L in
  foldi
    (fun acc1 j xs ->
      foldi
        (fun acc2 i _ ->
          filter_map (fun d -> match_xmas d (mk_coord i j) b) dirs @ acc2)
        acc1 xs)
    [] b

;;

parse_board "day4.input" |> search |> L.length;;
