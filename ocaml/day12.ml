open Utils

let p = P.(each_line (many any_char))

module Plot = struct
  type t = { name : char; p : Coord.t; mutable n_neighbors : int }
  let make name p = { name; p; n_neighbors = 0 }

  let hash p = Coord.hash p.p
  let compare p1 p2 = Coord.compare p1.p p2.p
  let equal p1 p2 = Coord.equal p1.p p2.p
end

module PG = Graph.Imperative.Graph.Concrete (Plot)
module SCC = Graph.Components.Make (PG)

let mk_plots input =
  P.parse_file p input |> R.get_exn |> LM.remove_empty_rows
  |> LM.mapij (fun y x c -> Plot.make c Coord.(make x y))

let build_graph plots =
  let g = PG.create () in
  LM.iterij
    (fun _ _ (p : Plot.t) ->
      let open L in
      PG.add_vertex g p;
      L.iter
        (fun d ->
          let p' = Coord.(p.p + d) in
          match LM.get plots p'.x p'.y with
          | None -> ()
          | Some (n : Plot.t) ->
              if CCChar.equal p.name n.name then (
                p.n_neighbors <- p.n_neighbors + 1;
                PG.add_edge g p n))
        Coord.compass)
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
