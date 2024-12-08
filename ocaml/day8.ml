open Utils

let p = P.(each_line @@ many any_char)

let enum_elements (board : char AM.t) =
  let open A in
  fold
    (fun acc1 xs ->
      fold
        (fun acc2 x ->
          if x <> '.' then
            L.add_nodup ~eq:Char.equal x acc2
          else
            acc2)
        acc1 xs)
    [] board

let enum_positions (board : char AM.t) =
  let open A in
  foldi
    (fun acc1 j xs ->
      foldi
        (fun acc2 i x ->
          if x <> '.' then
            (x, Coord.make i j) :: acc2
          else
            acc2)
        acc1 xs)
    [] board

let get_dict board =
  let open L in
  let pairs = enum_positions board in
  let* k = enum_elements board in
  pure
  @@ ( k,
       filter_map
         (fun (k', v) -> if Char.equal k k' then Some v else None)
         pairs )

let plot_two_antinodes board =
  ignore
  @@
  let dict = get_dict board in
  let open L in
  let* c, positions = dict in
  let* p1 = positions and* p2 = positions in
  let dx, dy = (p2.x - p1.x, p2.y - p1.y) in
  (* vector from p1 to p2 *)
  if not (dx = 0 && dy = 0) then (
    pr "%c: %s %s\n" c (Coord.to_string p1) (Coord.to_string p2);
    AM.set board (p1.x - dx) (p1.y - dy) '#';
    AM.set board (p2.x + dx) (p2.y + dy) '#');
  pure ()

let trace board ?(forward = true) ~(start : Coord.t) ~(direction : Coord.t) () =
  let open Coord in
  let d = (if forward then 1 else -1) *. direction in
  let rec go (p : Coord.t) =
    if O.is_some @@ AM.get board p.x p.y then (
      AM.set board p.x p.y '#';
      Utils.pr "--%s" (to_string p);
      go (p + d))
  in
  pr start;
  AM.set board start.x start.y '#';
  go (start + d);
  print_newline ()

let plot_many_antinodes board =
  ignore
  @@
  let dict = get_dict board in
  let open L in
  let* c, positions = dict in
  let* p1 = positions and* p2 = positions in
  let d = Coord.make (p2.x - p1.x) (p2.y - p1.y) in
  (* vector from p1 to p2 *)
  if not (d.x = 0 && d.y = 0) then (
    pr "%c: %s %s\n" c (Coord.to_string p1) (Coord.to_string p2);
    trace board ~forward:true ~start:p1 ~direction:d ();
    trace board ~forward:false ~start:p1 ~direction:d ());
  pure ()

let solve1 input =
  let board = get_input p input |> LM.remove_empty_rows |> AM.of_list in
  plot_two_antinodes board;
  let dict = get_dict board in
  L.(length (assoc ~eq:Char.equal '#' dict))

let solve2 input =
  let board = get_input p input |> LM.remove_empty_rows |> AM.of_list in
  plot_many_antinodes board;
  let dict = get_dict board in
  L.(length (assoc ~eq:Char.equal '#' dict))

let b = get_input p "8.txt" |> LM.remove_empty_rows |> AM.of_list;;

solve2 "day8.input"
