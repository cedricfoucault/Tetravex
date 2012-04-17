open Str
open Facile
open Easy

(* type color = int;; *)
(* type tile = color array;; *)

(* let default_color () = 0;;
let default_tile_ = Array.make 4 (default_color ());;
let default_tile () = default_tile_;;
let fresh_tile () = Array.copy (default_tile ());;
let set_tile p c1 c2 c3 c4 = begin
  p.(0) <- c1;
  p.(1) <- c2;
  p.(2) <- c3;
  p.(3) <- c4;
end;;
let right_color tile = tile.(0);;
let left_color tile = tile.(2);;
let top_color tile = tile.(1);;
let bottom_color tile = tile.(3);;
let copy_tile tile = Array.copy tile;;
let tile_to_string p = begin
  let concatenate_color str c = str ^ (Printf.sprintf "%d " c) in
  Array.fold_left concatenate_color "" p;
end;; *)

(* type tiles_set = tile array array;;

let fresh_tiles_set m n = begin
  Array.init m (fun _ -> Array.init n (fun _ -> fresh_tile ()))
end;;

let iter_tiles func tiles = begin
  Array.iter (Array.iter func) tiles
end;; *)
module Color = struct
  type t = int
  let default () = 0
  let none () = -1
  let random c = Random.int c
  let to_string c = string_of_int c
end;;

module Tile = struct
  type t = Color.t * Color.t * Color.t * Color.t
  let default () = 
    (Color.default(), Color.default(), Color.default(), Color.default())
  let empty () =
    (Color.none(), Color.none(), Color.none(), Color.none())
  let make cr ct cl cb = (cr, ct, cl, cb)
  let right_color (cr, _, _, _) = cr
  let top_color (_, ct, _, _) = ct
  let left_color (_, _, cl, _) = cl
  let bottom_color (_, _, _, cb) = cb
  (* let to_string tile = begin
      let concatenate_color str c = str ^ (Printf.sprintf "%d " c) in
      Array.fold_left concatenate_color "" p; *)
  let random c =
    (Color.random c, Color.random c, Color.random c, Color.random c)
  let to_string (cr, ct, cl, cb) =
    Printf.sprintf "%s %s %s %s" (Color.to_string cr) (Color.to_string ct)
      (Color.to_string cl) (Color.to_string cb)
  let copy tile = tile
end

module Puzzle = struct
  
  type t = {
    m: int;
    n: int;
    c: int;
    tiles: Tile.t array;
  }
  
  let make mi ni ci = {
    m = mi;
    n = ni;
    c = ci;
    tiles = Array.make (mi * ni) (Tile.empty())
  }
  
  let height p = p.m
  let width p = p.n
  let colors p = p.c
  
  let get p i j = p.tiles.(i * p.n + j)
  let set p i j tile = p.tiles.(i * p.n + j) <- tile
  
  let can_set p i j tile =
    let m = p.m and n = p.n and tiles = p.tiles in
    let left_match = 
      (j = 0) ||
      (Tile.left_color tile = Tile.right_color tiles.(i * n + j - 1)) in
    if left_match then
      let right_match =
        (j = (n - 1)) ||
        (Tile.right_color tile = Tile.left_color tiles.(i * n + j + 1)) in
      if right_match then
        let top_match =
          (i = 0) ||
          (Tile.top_color tile = Tile.bottom_color tiles.((i - 1) * n + j)) in
        if top_match then
          let bottom_match =
            (i = m - 1) ||
            (Tile.bottom_color tile = Tile.top_color tiles.((i + 1) * n + j))
          in bottom_match
        else false
      else false
    else false
          
  
  let to_string p = begin
    let m, n, c = p.m, p.n, p.c in
    let first_line = Printf.sprintf "%d %d %d" m n c in
    let offset = String.length first_line in
    let color_length = 1 + c / 10 in
    let line_length = 4 * (color_length + 1) in
    let str_length = (m * n) * line_length + offset in
    let str = String.make str_length '\n' in
    String.blit first_line 0 str 0 5;
    for i = 0 to m * n - 1 do
      let line = (Tile.to_string p.tiles.(i)) in
      String.blit line 0 str (offset + i * line_length) (String.length line);
    done;
    str;
  end
  (* let concatenate_tile str tile = 
    str ^ (Printf.sprintf "%s\n" (tile_to_string tile)) 
  in
  Array.fold_left concatenate_tile "" tiles; *)
  
  let read_file file = begin
    let blank_regexp = Str.regexp "[ \t]+" in
    (* read m, n, c from the first line *)
    let m = ref 0 and n = ref 0 and c = ref 0 in
    let read_first_line m n c = begin
      let fst_line = input_line file
      and set_from_str str intvar = intvar := (int_of_string str) in
      List.iter2 set_from_str (Str.split blank_regexp fst_line) [m; n; c];
    end in
    read_first_line m n c;
    (* read each  *)
    let puzzle = make !m !n !c in
    for i = 0 to !m * !n - 1 do
      let line = input_line file in
      let values = List.map int_of_string (Str.split blank_regexp line) in
      match values with
      | [c1; c2; c3; c4] -> puzzle.tiles.(i) <- (Tile.make c1 c2 c3 c4);
      | _ -> failwith "error during parsing...\n";
    done;
    puzzle;
  end
  
  let read filename = begin
    let file = open_in filename in
    let puzzle = read_file file in
    close_in file;
    puzzle;
  end
  
  let read_in () = begin
    read_file stdin;
  end
  
  let save p filename = begin
    let file = open_out filename in
    Printf.fprintf file "%d %d %d\n" p.m p.n p.c;
    let save_tile tile = Printf.fprintf file "%s\n" (Tile.to_string tile) in
    Array.iter save_tile p.tiles;
    flush file;
    close_out file;
  end
  
  let print p = begin
    Printf.printf "%d %d %d\n" p.m p.n p.c;
    Array.iter (fun t -> Printf.printf "%s\n" (Tile.to_string t)) p.tiles;
  end
  
  let copy puzzle = {
    m = puzzle.m;
    n = puzzle.n;
    c = puzzle.c;
    tiles = Array.copy puzzle.tiles;
  }
  
  exception No_Solution;;

  let get_solution puzzle = begin
    let m = puzzle.m and n = puzzle.n and tiles = puzzle.tiles in
    let mn = m * n in
    (* Finite domain: *)
    (* Each tile must be placed in one of the m * n position on the board *)
    let positions = Fd.array mn 0 (mn - 1) in
    (* Global constraint: *)
    (* No two tiles are at the same position *)
    Cstr.post (Alldiff.cstr positions);
    let lines = Array.map (fun p -> Arith.e2fd (fd2e p /~ i2e n)) positions
    and columns = Array.map (fun p -> Arith.e2fd (fd2e p %~ i2e n)) positions
    in
    (* Local constraints: *)
    (* Only two tiles that match can be next to each other *)
    try
      for pos1 = 0 to mn - 1 do
        (* right neighbor constraint *)
        let cr = ref (fd2e columns.(pos1) =~ (i2e (n - 1)))
        (* top neighbor constraint *)
        and ct = ref (fd2e lines.(pos1) =~ (i2e 0))
        in
        for pos2 = 0 to mn - 1 do
          if (pos1 <> pos2) then begin
            let tile1 = tiles.(pos1) and tile2 = tiles.(pos2) in
            (* if right color of tile1 and left color of tile2 match *)
            if (Tile.right_color tile1 = Tile.left_color tile2) then begin
              (* position of tile1 = position of tile2 - 1 *)
              cr := !cr ||~~
                (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e 1));
            end;
            (* if top color of tile1 and bottom color of tile2 match  *)
            if (Tile.top_color tile1 = Tile.bottom_color tile2) then begin
              (* line of tile1 = line of tile2 + 1 *)
              ct := !ct ||~~
                (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e n));
            end;
          end;
        done;
        Cstr.post !cr;
        Cstr.post !ct;
      done;
      (* Try to label every position by constraint solving *)
      if Goals.solve (Goals.Array.labeling positions) then begin
        (* if a solution has been found, return the array of positions *)
        Array.map Fd.elt_value positions
      end 
      else raise No_Solution
    with
    | _ -> raise No_Solution
  end;;

  (* let place_tiles problem solution = begin
    let m = problem.m and n = problem.n in
    let placed_tiles = Array.make (m * n) (Tile.default()) in
    for i = 0 to m * n - 1 do
      placed_tiles.(solution.(i)) <- Tile.copy problem.tiles.(i);
    done;
    placed_tiles;
  end;; *)

  let solve puzzle = begin
    let m = puzzle.m and n = puzzle.n in
    let solution = get_solution puzzle in
    let tiles_copy = Array.copy puzzle.tiles in
    for i = 0 to m * n - 1 do
      puzzle.tiles.(solution.(i)) <- tiles_copy.(i);
    done;
  end;;
  
  let get_solved puzzle = begin
    let solved_puzzle = copy puzzle in
    solve solved_puzzle;
    solved_puzzle;
  end
  
  let generate m n c = begin
    (* works for a puzzle of size at least 2x2 *)
    (* init the seed for the pseudo random generator *)
    Random.self_init ();
    (* generate a well-formed solution *)
    let puzzle = make m n c in
    let tiles = puzzle.tiles in
    (* constrained horizontal colors *)
    let hc =  Array.init (m * (n - 1)) (fun i -> Color.random c)
    (* constrained vertical colors *)
    and vc = Array.init (n * (m - 1)) (fun i -> Color.random c)
    in
    (* center tiles *)
    for i = 1 to (m - 2) do
      for j = 1 to (n - 2) do
        let hi = i * (n - 1) + j - 1
        and vi = j * (m - 1) + i - 1 in
        let tile = Tile.make hc.(hi + 1) vc.(vi) hc.(hi) vc.(vi + 1) in
        tiles.(i * n + j) <- tile;
      done;
    done;
    (* top and bottom tiles *)
    for j = 1 to n - 2 do
      (* top tile: i = 0 *)
      let hi = j - 1
      and vi = j * (m - 1) - 1 in
      (* top color isn't constrained (random) *)
      let tile = Tile.make hc.(hi + 1) (Color.random c) hc.(hi) vc.(vi + 1) in
      tiles.(j) <- tile;
      (* bottom tile: i = m - 1 *)
      let hi = (m - 1) * (n - 1) + j - 1
      and vi = j * (m - 1) + m - 2 in
      (* bottom color isn't contrained (random) *)
      let tile = Tile.make hc.(hi + 1) vc.(vi) hc.(hi) (Color.random c) in
      tiles.((m - 1) * n + j) <- tile;
    done;
    (* left and right tiles *)
    for i = 1 to m - 2 do
      (* left tile: j = 0 *)
      let hi = i * (n - 1) - 1
      and vi = i - 1 in
      (* left color isn't constrained (random) *)
      let tile = Tile.make hc.(hi + 1) vc.(vi) (Color.random c) vc.(vi + 1) in
      tiles.(i * n) <- tile;
      (* right tile: j = n - 1 *)
      let hi = i * (n - 1) + n - 2
      and vi = (n - 1) * (m - 1) + i - 1 in
      (* right color isn't contrained (random) *)
      let tile = Tile.make (Color.random c) vc.(vi) hc.(hi) vc.(vi + 1) in
      tiles.(i * n + (n - 1)) <- tile;
    done;
    (* corner tiles *)
    (* top left hand corner *)
    tiles.(0) <- 
      Tile.make hc.(0) (Color.random c) (Color.random c) vc.(0);
    (* top right hand corner *)
    tiles.(n - 1) <-
      Tile.make (Color.random c) (Color.random c) hc.(n - 2) vc.((n - 1) * (m - 1));
    (* bottom left hand corner *)
    tiles.((m - 1) * n) <-
      Tile.make hc.((m - 1) * (n - 1)) vc.(m - 2) (Color.random c) (Color.random c);
    (* bottom right hand corner *)
    tiles.((m - 1) * n + (n - 1)) <-
      Tile.make (Color.random c) vc.((n - 1) * (m - 1) + m - 2) hc.((m - 1) * (n - 1) + n - 2) (Color.random c);
      
    (* then shuffle the puzzle randomly *)
    let shuffle tiles = Array.sort (fun _ _ -> Random.int 3 - 1) tiles in
    shuffle tiles;
    puzzle;
  end
  
  let dumb_generate mi ni ci = {
    m = mi;
    n = ni;
    c = ci;
    tiles = Array.init (mi * ni) (fun _ -> Tile.random ci);
  }
end;;

(* type tiles_set = tile array;;

let empty_tiles_set m n = begin
  Array.make (m * n) (default_tile ())
end;;
let fresh_tiles_set m n = begin
  Array.init (m * n) (fun _ -> fresh_tile ())
end;;

let iter_tiles = Array.iter;;

let nth_tile tiles n = tiles.(n);;

let tiles_to_string tiles = begin
  let concatenate_tile str tile = 
    str ^ (Printf.sprintf "%s\n" (tile_to_string tile)) 
  in
  Array.fold_left concatenate_tile "" tiles;
end

let print_tiles tiles = begin
  iter_tiles (fun p -> Printf.printf "%s\n" (tile_to_string p)) tiles
end;;

let copy_tiles tiles = Array.copy tiles;;

type problem = {
  m: int;
  n: int;
  c: int;
  tiles: tiles_set;
};;

let make_problem mi ni ci = {
  m = mi;
  n = ni;
  c = ci;
  tiles = fresh_tiles_set mi ni;
};;

let print_problem prob = begin
  Printf.printf "number of rows m: %d\n" prob.m;
  Printf.printf "number of columns n: %d\n" prob.n;
  Printf.printf "number of colors c: %d\n" prob.c;
  Printf.printf "Set of tiles:\n";
  print_tiles prob.tiles;
end;;

let read_problem filename = begin
  let file = open_in filename in
  let blank_regexp = Str.regexp "[ \t]+" in
  (* read m, n, c from the first line *)
  let m = ref 0 and n = ref 0 and c = ref 0 in
  let read_first_line m n c = begin
    let fst_line = input_line file
    and set_from_str str intvar = intvar := (int_of_string str) in
    List.iter2 set_from_str (Str.split blank_regexp fst_line) [m; n; c];
  end in
  read_first_line m n c;
  (* read each  *)
  let problem = make_problem !m !n !c in
  let set_tile_from_line p =
    let line = input_line file in
    let values = List.map int_of_string (Str.split blank_regexp line) in
    match values with
    | [c1; c2; c3; c4] -> set_tile p c1 c2 c3 c4
    | _ -> failwith (Printf.sprintf "error parsing %s" filename)
  in
  iter_tiles set_tile_from_line problem.tiles;
  close_in file;
  problem;
end;;

let save_problem prob filename = begin
  let file = open_out filename in
  Printf.fprintf file "%d %d %d\n" prob.m prob.n prob.c;
  Printf.fprintf file "%s\n" (tiles_to_string prob.tiles);
  flush file;
  close_out file;
end

(* solution : list of position of each tile on the board *)
type solution = int array;;
exception No_Solution;;

let get_solution problem = begin
  let m = problem.m and n = problem.n and tiles = problem.tiles in
  let mn = m * n in
  (* Finite domain: *)
  (* Each tile must be placed in one of the m * n position on the board *)
  let positions = Fd.array mn 0 (mn - 1) in
  (* Global constraint: *)
  (* No two tiles are at the same position *)
  Cstr.post (Alldiff.cstr positions);
  let lines = Array.map (fun p -> Arith.e2fd (fd2e p /~ i2e n)) positions
  and columns = Array.map (fun p -> Arith.e2fd (fd2e p %~ i2e n)) positions
  in
  (* Local constraints: *)
  (* Two tiles whose color don't match can't be next to each other *)
  for pos1 = 0 to mn - 1 do
    (* right neighbor constraint *)
    let cr = ref (fd2e columns.(pos1) =~ (i2e (n - 1)))
    (* top neighbor constraint *)
    and ct = ref (fd2e lines.(pos1) =~ (i2e 0))
    in
    for pos2 = 0 to mn - 1 do
      if (pos1 <> pos2) then begin
        let tile1 = nth_tile tiles pos1 and tile2 = nth_tile tiles pos2 in
        (* if right color of tile1 and left color of tile2 match *)
        if (right_color tile1 = left_color tile2) then begin
          (* position of tile1 = position of tile2 - 1 *)
          cr := !cr ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e 1));
        end;
        (* if top color of tile1 and bottom color of tile2 match  *)
        if (top_color tile1 = bottom_color tile2) then begin
          (* line of tile1 = line of tile2 + 1 *)
          ct := !ct ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e n));
        end;
      end;
    done;
    Cstr.post !cr;
    Cstr.post !ct;
  done;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then begin
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  end 
  else raise No_Solution
end;;

let place_tiles problem solution = begin
  let m = problem.m and n = problem.n in
  let placed_tiles = empty_tiles_set m n in
  for i = 0 to m * n - 1 do
    placed_tiles.(solution.(i)) <- copy_tile (nth_tile problem.tiles i);
  done;
  placed_tiles;
end;;

let solve problem = begin
  let m = problem.m and n = problem.n in
  let solution = get_solution problem in
  let tiles_copy = copy_tiles problem.tiles in
  for i = 0 to m * n - 1 do
    problem.tiles.(solution.(i)) <- tiles_copy.(i);
  done;
end;; *)

(* let puzzle = Puzzle.read "exemple1" in begin
  Puzzle.print puzzle;
  Puzzle.solve puzzle;
  Printf.printf "Solution:\n";
  Puzzle.print puzzle;
end;;
let puzzle = Puzzle.read "exemple3x3" in begin
  Puzzle.print puzzle;
  Puzzle.save puzzle "exemple3x3_copy";
  let t = Sys.time() in
  Puzzle.solve puzzle;
  let dtime = Sys.time() -. t in
  Printf.printf "Solution:\n";
  Puzzle.print puzzle;
  Printf.printf "Took %fs\n" dtime;
end;;
let puzzle = Puzzle.read "exemple4x4" in begin
  Puzzle.print puzzle;
  flush stdout;
  let t = Sys.time() in
  Puzzle.solve puzzle;
  let dtime = Sys.time() -. t in
  Printf.printf "Solution:\n";
  Puzzle.print puzzle;
  Printf.printf "Took %fs\n" dtime;
end;;
let puzzle = Puzzle.generate 4 4 10 in begin
  Puzzle.print puzzle;
  flush stdout;
  let t = Sys.time() in
  Puzzle.solve puzzle;
  let dtime = Sys.time() -. t in
  Printf.printf "Solution:\n";
  Puzzle.print puzzle;
  Printf.printf "Took %fs\n" dtime;
end;;
let puzzle = Puzzle.dumb_generate 4 4 10 in begin
  Puzzle.print puzzle;
  flush stdout;
  let t = Sys.time() in
  Puzzle.solve puzzle;
  let dtime = Sys.time() -. t in
  Printf.printf "Solution:\n";
  Puzzle.print puzzle;
  Printf.printf "Took %fs\n" dtime;
end *)

