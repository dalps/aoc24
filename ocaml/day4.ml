module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult

type dir_x = L | C | R
type dir_y = T | C | B
type dir = { dx : dir_x; dy : dir_y }

type coord = { x : int; y : int }

let string_of_coord p = Printf.sprintf "{%d,%d}" p.x p.y

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

let match_goal (goal : string) (d : dir) (p : coord) (board : char list list) =
  let d = coord_of_dir d in
  let open L in
  let path = combine (CCString.to_list goal) (0 --^ CCString.length goal) in
  let open O in
  let* _ =
    assume
    @@ L.for_all
         (fun (c, step) ->
           match
             lookup (mk_coord (p.x + (d.x * step)) (p.y + (d.y * step))) board
           with
           | Some x when CCChar.equal x c -> true
           | _ -> false)
         path
  in
  return (mk_solution d p)

let match_xmas = match_goal "XMAS"

let match_x_mas (p : coord) (board : char list list) =
  let open O in
  let open CCChar in
  let* a = lookup (mk_coord p.x p.y) board in
  let* _ = assume (a = 'A') in
  let* tl = lookup (mk_coord (p.x - 1) (p.y - 1)) board in
  let* tr = lookup (mk_coord (p.x + 1) (p.y - 1)) board in
  let* bl = lookup (mk_coord (p.x - 1) (p.y + 1)) board in
  let* br = lookup (mk_coord (p.x + 1) (p.y + 1)) board in
  let* _ =
    assume
      (tl <> br && tr <> bl
      && L.for_all (fun x -> x = 'M' || x = 'S') [ tl; tr; bl; br ])
  in
  Printf.printf "%c.%c\n.%c.\n%c.%c\n%s\n\n" tl tr a bl br (string_of_coord p);
  return p

let p = P.(each_line (many any_char))

let parse_board input =
  P.parse_file p input |> Result.get_ok |> L.filter (fun l -> l <> [])

let search1 (b : char list list) =
  let open L in
  foldi
    (fun acc1 j xs ->
      foldi
        (fun acc2 i _ ->
          filter_map (fun d -> match_xmas d (mk_coord i j) b) dirs @ acc2)
        acc1 xs)
    [] b

let search2 (b : char list list) =
  let open L in
  foldi
    (fun acc1 j xs ->
      foldi (fun acc2 i _ -> match_x_mas (mk_coord i j) b :: acc2) acc1 xs)
    [] b
  |> keep_some

let b = parse_board "day4.input";;
b |> search1 |> L.length;;
b |> search2 |> L.length (* 1st: 17160; 2nd: 2145 *)
