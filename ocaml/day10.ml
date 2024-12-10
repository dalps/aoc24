open Utils

let p = P.(each_line (many any_char))

module Level = struct
  type t = { level : int; coord : Coord.t }
  let compare l1 l2 = Coord.compare l1.coord l2.coord
  let hash l = Coord.hash l.coord
  let equal l1 l2 = Coord.equal l1.coord l2.coord
  let level l = l.level
  let x l = l.coord.x
  let y l = l.coord.y
  let make level coord = { level; coord }
  let make' level x y = { level; coord = { x; y } }
  let to_string l = spr "{%d, (%d,%d)}" l.level l.coord.x l.coord.y
end

module GTrail = Graph.Imperative.Digraph.Concrete (Level)

let get_topo input =
  input |> P.parse_file p |> R.get_exn |> LM.remove_empty_rows
  |> LM.map CCChar.to_string |> LM.map CCInt.of_string_exn

let compass = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] (* NESW (column,row) *)

let get_trail_levels l m =
  LM.filter_mapij
    (fun y x level ->
      if level = l then
        Some (Level.make' level x y)
      else
        None)
    m

let build_graph m =
  let g = GTrail.create () in
  let open ListMatrix in
  iterij
    (fun y x level ->
      L.iter
        (fun (dx, dy) ->
          ignore
          @@
          let open O in
          let d = Coord.make (x + dx) (y + dy) in
          let* level' = get m d.x d.y in
          if level' - level = 1 then
            GTrail.add_edge g (Level.make' level x y) (Level.make level' d);
          pure ())
        compass)
    m;
  g

let levels = get_topo "10.txt"
let g = build_graph levels

let solve1 input =
  let m = get_topo input in
  let g = build_graph m in
  let heads = get_trail_levels 0 m in
  let tops = get_trail_levels 9 m in
  let module PC = Graph.Path.Check (GTrail) in
  let pc = PC.create g in
  let result = ref 0 in
  let open L in
  iter
    (fun h -> iter (fun t -> if PC.check_path pc h t then incr result) tops)
    heads;
  !result
;;

GTrail.iter_edges
  (fun v1 v2 -> pr "%s->%s\n" (Level.to_string v1) (Level.to_string v2))
  g