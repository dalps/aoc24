open Utils

module G = Graph.Imperative.Graph.Concrete (Coord)

let p = P.(each_line (many any_char))

let get_string_input input =
  P.parse_string_exn p input |> LM.remove_empty_rows |> AM.of_list

let get_input input =
  P.parse_file_exn p input |> LM.remove_empty_rows |> AM.of_list

let get_graph board =
  let open AM in
  let open Coord in
  let g = G.create () in
  iterp
    (fun p -> function
      | '.' | 'S' | 'E' ->
          G.add_vertex g p;
          A.iter
            (fun dir ->
              if board.-(p + dir) <> Some '#' then G.add_edge g p (p + dir))
            compass
      | _ -> ())
    board;
  g

type cost = INFINITY | REAL of int

module Cost = struct
  type t = cost
  let ( +. ) c n = match c with INFINITY -> INFINITY | REAL c -> REAL (c + n)

  let ( + ) c1 c2 =
    match (c1, c2) with
    | INFINITY, _ | _, INFINITY -> INFINITY
    | REAL n1, REAL n2 -> REAL (n1 + n2)

  let zero = REAL 0

  let max = INFINITY

  let compare c1 c2 =
    match (c1, c2) with
    | INFINITY, _ -> 1
    | _, INFINITY -> -1
    | REAL n1, REAL n2 -> Int.compare n1 n2

  let show = function INFINITY -> "inf" | REAL c -> string_of_int c

  let ( <= ) c1 c2 = compare c1 c2 <= 0
  let ( < ) c1 c2 = compare c1 c2 < 0
end

let cost prev curr next =
  if Coord.aligned [ prev; curr; next ] then
    1
  else
    1000 + 1

module State = struct
  type t = { p : Coord.t; cost : cost; dir : Coord.t }

  let mk p cost dir = { p; cost; dir }

  let leq u1 u2 = Cost.compare u1.cost u2.cost <= 0
end
module H = CCHeap.Make (State)
module D = CCHashtbl.Make (Coord)

let dijkstra g source =
  let q = ref H.empty in
  let dist = D.create (G.nb_vertex g) in
  let prev = D.create (G.nb_vertex g) in
  G.iter_vertex
    (fun p ->
      if not (Coord.equal p source) then (
        D.add dist p INFINITY;
        D.add prev p None;
        q := H.add !q (State.mk p INFINITY Coord.east)))
    g;
  q := H.add !q (State.mk source Cost.zero Coord.east);
  D.add dist source Cost.zero;

  while not (H.is_empty !q) do
    pr "n unvisited: %d\n" (H.size !q);
    let q', u = H.take_exn !q in
    pr "min: %8s with distance %s from %s\n" (Coord.show u.p) (Cost.show u.cost)
      (Coord.string_of_dir u.dir);
    q := q';

    G.iter_succ
      (fun v ->
        pr "- considering neighbor %8s: " (Coord.show v);
        (* distance from source to v passing through u *)
        let c = cost Coord.(u.p - u.dir) u.p v in
        let alt = Cost.(u.cost +. c) in
        (* known distance from source to v *)
        let distv = D.get_or dist v ~default:INFINITY in
        pr "  %s <? %s " (Cost.show alt) (Cost.show distv);
        if Cost.(alt < distv) then (
          (* found a better path *)
          pr "  yeah, update";
          D.replace dist v alt;
          D.replace prev v (Some u.p);
          q :=
            H.delete_one
              (fun s1 s2 -> Coord.(s1.p =. s2.p))
              State.(mk v INFINITY Coord.east)
              !q;
          q := H.add !q (State.mk v alt (Coord.( - ) v u.p)));
        pr "\n")
      g u.p
  done;
  (dist, prev)

let rec get_path prevs source target =
  let rec go accu u =
    pr "%s --> " (Coord.show u);
    match D.find prevs u with
    | Some _ when Coord.(u =. source) -> accu
    | Some v -> go (u :: accu) v
    | None -> accu
  in
  go [] target

let solve1 input =
  let b = get_input input in
  let g = get_graph b in
  let open Coord in
  let open AM in
  let source =
    find (C.equal 'S') b |> O.get_exn_or "no player" |> Coord.of_pair
  in
  let target =
    find (C.equal 'E') b |> O.get_exn_or "no player" |> Coord.of_pair
  in
  let dists, prevs = dijkstra g source in
  D.find dists target

let b1 = get_input (data "16a.txt")
let b2 = get_input (data "16b.txt")
let boss = get_input (data "day16.input")

let%expect_test "dijkstra" =
  let g = get_graph b1 in
  let dists, prevs = dijkstra g (Coord.make 1 13) in
  D.iter (fun p c -> pr "%8s %8s\n" (Coord.show p) (Cost.show c)) dists;
  D.iter
    (fun p p' ->
      pr "%8s --> %-8s\n" (Coord.show p)
        (O.map_or Coord.show ~default:"none" p'))
    prevs;
  let source = Coord.make 1 13 in
  let target = Coord.make 13 1 in
  pr "%s" (Cost.show (D.find dists target));
  let path = get_path prevs source target in
  L.iter
    (fun p ->
      AM.(b1.-(p) <- 'o');
      pr "%s --> " (Coord.show p))
    path;
  AM.print_board ~print_a:C.to_string b1

let%expect_test "" =
  let open Coord in
  let open AM in
  let g = get_graph b1 in
  print_board ~print_a:CCChar.to_string b1;
  G.succ g (Coord.make 1 13) |> L.iter (fun p1 -> pr "%s\n" (show p1));
  G.succ g (Coord.make 1 12) |> L.iter (fun p1 -> pr "%s\n" (show p1));
  G.succ g (Coord.make 3 7) |> L.iter (fun p1 -> pr "%s\n" (show p1));
  G.fold_edges (fun p1 p2 acc -> (p1, p2) :: acc) g []
  |> L.sort (fun (p1, p2) (q1, q2) -> compare p1 q1)
  |> L.iter (fun (p1, p2) -> pr "%s --> %s\n" (show p1) (show p2))

let%expect_test "solve1" =
  let cost = solve1 (data "16a.txt") in
  pr "%s\n" (Cost.show cost)
