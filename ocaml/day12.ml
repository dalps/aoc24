open Utils

let p = P.(each_line (many any_char))

module Plot = struct
  type t = {
    name : char;
    p : Coord.t;
    mutable n_neighbors : int;
    boundary : bool array;
  }
  [@@deriving show]

  let make name p = { name; p; n_neighbors = 0; boundary = A.make 4 false }

  let hash p = Coord.hash p.p
  let compare p1 p2 = Coord.compare p1.p p2.p
  let equal p1 p2 = Coord.equal p1.p p2.p
end

module PG = Graph.Imperative.Graph.Concrete (Plot)
module SCC = Graph.Components.Make (PG)

let mk_plots input =
  P.parse_file p input |> R.get_exn |> LM.remove_empty_rows
  |> LM.mapij (fun y x c -> Plot.make c (Coord.make x y))

let set_wrap (a : bool array) (i : int) = a.(i mod A.length a) <- true

let show_compass = function
  | 0 -> "north"
  | 1 -> "east"
  | 2 -> "south"
  | _ -> "west"

let build_graph plots =
  let g = PG.create () in
  LM.iterij
    (fun _ _ (p : Plot.t) ->
      PG.add_vertex g p;
      L.iteri
        (fun i d ->
          (* 0 is north, clockwise *)
          let p' = Coord.(p.p + d) in
          match LM.get plots p'.x p'.y with
          | Some (n : Plot.t) when CCChar.equal p.name n.name ->
              p.n_neighbors <- p.n_neighbors + 1;
              PG.add_edge g p n
          | Some other ->
              Format.printf
                "Facing %s, I found that @.%c at %a borders with @.%c at %a\n"
                (show_compass i) p.name Coord.pp p.p other.name Coord.pp other.p;
              set_wrap p.boundary i;
              set_wrap other.boundary (i + 2)
          | None -> set_wrap p.boundary i)
        Coord.compass0)
    plots;
  g

let solve1 input =
  let plots = mk_plots input in
  let g = build_graph plots in
  let scc = SCC.scc_list g in
  L.fold_left
    (fun acc ps ->
      let peri, area =
        L.fold_left
          (fun (peri, area) (p : Plot.t) ->
            pr "%c " p.name;
            (peri + (4 - p.n_neighbors), area + 1))
          (0, 0) ps
      in
      pr "perimeter: %d, area: %d\n" peri area;
      acc + (peri * area))
    0 scc

let rec get_side_aux x y ((side, sides) as accu) (plots : Plot.t list) =
  match plots with
  | [] -> accu
  | [ p ] -> ([], (p :: side) :: sides)
  | p1 :: p2 :: ps ->
      let accu' =
        if x p1.p = x p2.p && abs (y p1.p - y p2.p) = 1 then
          (* continue current side *)
          (p1 :: side, sides)
        else (* start a new side *)
          ([], (p1 :: side) :: sides)
      in
      get_side_aux x y accu' (p2 :: ps)

let get_sides x y (plots : Plot.t list) : Plot.t list list =
  get_side_aux x y ([], []) plots |> snd

let order x y (p1 : Plot.t) (p2 : Plot.t) =
  let p1, p2 = (p1.p, p2.p) in
  let c0 = Int.compare (x p1) (x p2) in
  if c0 = 0 then Int.compare (y p1) (y p2) else c0

let solve2 input =
  let open L in
  let plots = mk_plots input in
  let g = build_graph plots in
  let scc = SCC.scc_list g in
  fold_left
    (fun acc region ->
      (* Format.printf "region = @[%a@]@." (pp Plot.pp) region; *)
      (* for every kind of face (north, east...) *)
      let area = length region in
      let peri = ref 0 in
      for i = 0 to 3 do
        let boundary = filter (fun (p : Plot.t) -> p.boundary.(i)) region in
        let x', y' = Coord.(if i mod 2 = 0 then (y, x) else (x, y)) in
        let order (p1 : Plot.t) (p2 : Plot.t) =
          let p1, p2 = (p1.p, p2.p) in
          let c0 = Int.compare (x' p1) (x' p2) in
          if c0 = 0 then Int.compare (y' p1) (y' p2) else c0
        in
        let sorted = sort order boundary in
        Format.printf "b = @[%a@]@." (pp Plot.pp) sorted;
        let sides = sorted |> get_sides x' y' >|= L.length in
        let (sample : Plot.t) = hd region in
        Format.printf
          "For region %c at %a, facing %s, I found the following sides @[%a@]@."
          sample.name Coord.pp sample.p (show_compass i) (pp CCInt.pp) sides;

        peri := !peri + length sides
      done;
      acc + (area * !peri))
    0 scc
