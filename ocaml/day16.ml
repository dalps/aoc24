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

module OpenSet = CCSet.Make (struct
  type t = { p : Coord.t; cost : cost }

  let compare u1 u2 = Cost.compare u1.cost u2.cost
end)

module M = CCMap.Make (Coord)
module State = struct
  type t = { p : Coord.t; cost : cost }

  let mk p cost = { p; cost }

  let leq u1 u2 = Cost.compare u1.cost u2.cost <= 0
end
module H = CCHeap.Make (State)
module D = CCHashtbl.Make (Coord)
let dijkstra board g source =
  let q = ref H.empty in
  let dist = D.create (G.nb_vertex g) in
  let prev = D.create (G.nb_vertex g) in
  G.iter_vertex
    (fun p ->
      if not (Coord.equal p source) then (
        D.add dist p INFINITY;
        D.add prev p None;
        q := H.add !q (State.mk p INFINITY)))
    g;
  q := H.add !q (State.mk source Cost.zero);
  D.add dist source Cost.zero;

  while not (H.is_empty !q) do
    pr "n unvisited: %d\n" (H.size !q);
    let q', u = H.take_exn !q in
    pr "min: %8s with distance %s\n" (Coord.show u.p) (Cost.show u.cost);
    q := q';

    G.iter_succ
      (fun v ->
        pr "- considering neighbor %8s: " (Coord.show v);
        (* distance from source to v passing through u *)
        let alt = Cost.(u.cost +. 1) in
        (* known distance from source to v *)
        let distv = D.get_or dist v ~default:INFINITY in
        pr "  %s <=? %s " (Cost.show alt) (Cost.show distv);
        if Cost.(alt <= distv) then (
          (* found a better path *)
          pr "  yeah, update";
          D.replace dist v alt;
          D.replace prev v (Some u.p);
          q :=
            H.delete_one
              (fun s1 s2 -> Coord.(s1.p =. s2.p))
              State.(mk v INFINITY)
              !q;
          q := H.add !q (State.mk v alt));
        pr "\n")
      g u.p
  done;
  (dist, prev)

let cost prev curr next =
  if Coord.aligned [ prev; curr; next ] then
    1
  else
    1000 + 1

(* Why does the version with caching miss some results? There are as many results as incoming directions to 'E'. Does it have to do with checking that a cycle be reached *with* the same direction? What am I missing? *)

let rec dfs_aux cache visited board dir curr : (int * Coord.t list) list =
  let open Coord in
  let open AM in
  match board.-(curr) with
  | None -> failwith "out of bounds"
  | Some 'E' -> [ (0, []) ]
  | Some '#' -> []
  | _ when Hashtbl.mem visited curr -> []
  | _ ->
      Hashtbl.add visited curr ();
      let moves =
        A.filter_map
          (fun d ->
            if (not Coord.(equal d dir)) && board.-(d + curr) <> Some '#' then
              Some (cost (curr - dir) curr (curr + d), d)
            else
              None)
          compass
      in
      let open L in
      let* w, m = moves |> A.to_list in
      let next_dir = flip m in
      let next_pos = curr + m in
      let l =
        match Hashtbl.find_opt cache (next_dir, next_pos) with
        | Some l -> l
        | None ->
            let l =
              dfs_aux cache (Hashtbl.copy visited) board next_dir next_pos
            in
            Hashtbl.add cache (next_dir, next_pos) l;
            l
      in
      let* weight, path = l in
      return (Int.add w weight, next_pos :: path)

let discover board =
  let visited = Hashtbl.create A.(length board * A.length board.(0)) in
  let cache = Hashtbl.create 99999 in
  let open Coord in
  let open AM in
  let deer = find (C.equal 'S') board |> O.get_exn_or "no player" |> of_pair in
  let paths =
    dfs_aux cache visited board east deer
    |> L.sort (fun (w1, _) (w2, _) -> -Int.compare w1 w2)
  in
  L.iter
    (fun (w, p) ->
      let open A in
      let b' = map copy board |> copy in
      L.iter (fun c -> b'.-(c) <- 'o') p;
      print_board ~print_a:C.to_string b';
      pr "weight: %d\n" w)
    paths;
  paths

let b1 = get_input (data "16.txt")
let b2 = get_input (data "16b.txt")
let boss = get_input (data "day16.input")

let b' =
  {|
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############|}
  |> get_string_input

let%expect_test "dijkstra" =
  let g = get_graph b1 in
  let dists, prevs = dijkstra b1 g (Coord.make 1 13) in
  D.iter (fun p c -> pr "%8s %8s\n" (Coord.show p) (Cost.show c)) dists;
  D.iter
    (fun p p' ->
      pr "%8s --> %-8s\n" (Coord.show p)
        (O.map_or Coord.show ~default:"none" p'))
    prevs;
  let source = Coord.make 1 13 in
  let target = Coord.make 13 1 in
  let rec get_path accu u =
    pr "%s --> " (Coord.show u);
    match D.find prevs u with
    | Some _ when Coord.(u =. source) -> accu
    | Some v -> get_path (u :: accu) v
    | None -> accu
  in

  let path = get_path [] target in
  pr "hello %d\n" (L.length path);
  L.iter (fun p -> AM.(b1.-(p) <- 'o'); pr "%s --> " (Coord.show p)) path;
  AM.print_board ~print_a:C.to_string b1

  (* let%expect_test "" =
     let open Coord in
     let open AM in
     let g = get_graph b1 in
     print_board ~print_a:CCChar.to_string b1; *)
  (* G.succ g (Coord.make 1 13) |> L.iter (fun p1 -> pr "%s\n" (show p1));
     G.succ g (Coord.make 1 12) |> L.iter (fun p1 -> pr "%s\n" (show p1));
     G.succ g (Coord.make 3 7) |> L.iter (fun p1 -> pr "%s\n" (show p1)); *)
  (* G.fold_edges (fun p1 p2 acc -> (p1, p2) :: acc) g []
     |> L.sort (fun (p1, p2) (q1, q2) -> compare p1 q1)
     |> L.iter (fun (p1, p2) -> pr "%s --> %s\n" (show p1) (show p2)); *)
