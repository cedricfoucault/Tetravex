open Str;;
open Facile;;
open Easy;;
(* init the seed for the pseudo random generation *)
Random.self_init ();;

module Color = struct
  type t = int
  let default ()  = 0
  let none ()     = -1
  let random c    = Random.int c
  let to_string c = string_of_int c
  let to_int c    = c
  let of_int i    = i
end;;

module Tile = struct
  type t = Color.t * Color.t * Color.t * Color.t
  let default () = 
    (Color.default(), Color.default(), Color.default(), Color.default())
  let empty ()   = 
    (Color.none(), Color.none(), Color.none(), Color.none())
  let is_empty t = (t = empty ())
  let make cr ct cl cb = (cr, ct, cl, cb)
  let right_color (cr, _, _, _)  = cr
  let top_color (_, ct, _, _)    = ct
  let left_color (_, _, cl, _)   = cl
  let bottom_color (_, _, _, cb) = cb
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
  
  let copy puzzle = {
    m = puzzle.m;
    n = puzzle.n;
    c = puzzle.c;
    tiles = Array.copy puzzle.tiles;
  }
  
  let fill dest src = begin
    let dest_tiles = dest.tiles and src_tiles = src.tiles 
    and m = dest.m and n = dest.n in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        dest_tiles.(i * n + j) <- src_tiles.(i * n + j);
      done
    done
  end
  
  let empty p = begin
    let m, n, tiles = p.m, p.n, p.tiles in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        tiles.(i * n + j) <- Tile.empty ();
      done
    done
  end
  
  let is_full p = begin
    let m, n, tiles = p.m, p.n, p.tiles in
    let full = ref true in
    for i = 0 to n * m - 1 do
      if (Tile.is_empty tiles.(i)) then
        full := false;
    done;
    !full;
  end
  
  let height p = p.m
  let width p = p.n
  let colors p = p.c
  
  let get p i j = p.tiles.(i * p.n + j)
  let set p i j tile = p.tiles.(i * p.n + j) <- tile
  
  let can_set p i j tile =
    let m = p.m and n = p.n and tiles = p.tiles in
    if (Tile.is_empty tiles.(i * n + j)) then
      let left_match = 
        (j = 0) || Tile.is_empty tiles.(i * n + j - 1) ||
        (Tile.left_color tile = Tile.right_color tiles.(i * n + j - 1)) in
      if left_match then
        let right_match =
          (j = (n - 1)) || Tile.is_empty tiles.(i * n + j + 1) ||
          (Tile.right_color tile = Tile.left_color tiles.(i * n + j + 1)) in
        if right_match then
          let top_match =
            (i = 0) || Tile.is_empty tiles.((i - 1) * n + j) ||
            (Tile.top_color tile = Tile.bottom_color tiles.((i - 1) * n + j))
            in
          if top_match then
            (i = m - 1) || Tile.is_empty tiles.((i + 1) * n + j) ||
            (Tile.bottom_color tile = Tile.top_color tiles.((i + 1) * n + j))
          else false
        else false
      else false
    else false
          
  
  let to_string p = begin
    let m, n, c = p.m, p.n, p.c in
    let first_line = Printf.sprintf "%d %d %d" m n c in
    let offset = String.length first_line in
    let color_length = 1 + c / 10 in
    let line_length  = 4 * (color_length + 1) in
    let str_length   = (m * n) * line_length + offset in
    let str = String.make str_length '\n' in
    String.blit first_line 0 str 0 5;
    for i = 0 to m * n - 1 do
      let line = (Tile.to_string p.tiles.(i)) in
      String.blit line 0 str (offset + i * line_length) (String.length line);
    done;
    str;
  end
  
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
      let line   = input_line file in
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
  
  exception No_Solution;;
  
  (* [get_solution p]: finds a solution of p if it exists:
  returns an array indicating the position of each tile if it does
  or raise the exception No_Solution if it doesn't *)
  let get_solution puzzle = begin
    let m  = puzzle.m and n = puzzle.n and tiles = puzzle.tiles in
    let mn = m * n in
    (* Finite domain: *)
    (* Each tile must be placed in one of the m * n position on the board *)
    let positions = Fd.array mn 0 (mn - 1) in
    (* Global constraint: *)
    (* No two tiles are at the same position *)
    Cstr.post (Alldiff.cstr positions);
    let lines   = Array.map (fun p -> Arith.e2fd (fd2e p /~ i2e n)) positions
    and columns = Array.map (fun p -> Arith.e2fd (fd2e p %~ i2e n)) positions
    in
    (* Local constraints: *)
    (* Two tiles can be next to each other
       only if their adjacent faces match *)
    try
      for pos1 = 0 to mn - 1 do
        (* Init the right neighbor constraint:
          possibility of being next to the right edge *)
        let cr = ref (fd2e columns.(pos1) =~ (i2e (n - 1)))
        (* Init the top neighbor constraint:
          possibility of being next to the top edge *)
        and ct = ref (fd2e lines.(pos1) =~ (i2e 0))
        in
        for pos2 = 0 to mn - 1 do
          if (pos1 <> pos2) then begin
            let tile1 = tiles.(pos1) and tile2 = tiles.(pos2) in
            (* if right color of tile1 and left color of tile2 match *)
            if (Tile.right_color tile1 = Tile.left_color tile2) then begin
              (* add possibility position of tile1 = position of tile2 - 1 *)
              cr := !cr ||~~
                (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e 1));
            end;
            (* if top color of tile1 and bottom color of tile2 match  *)
            if (Tile.top_color tile1 = Tile.bottom_color tile2) then begin
              (* add possibility position of tile1 = position of tile2 + n *)
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
  
  exception Out_Of_Loop;;
  
  let is_solved puzzle = begin
    let m = puzzle.m and n = puzzle.n and tiles = puzzle.tiles in
    (* is_valid_line:
      checks the validity of each line recursively *)
    let rec is_valid_line tiles i = begin match (i = m - 1) with
    | true -> begin
        for j = 0 to (n - 2) do
          let tile       = tiles.(i * n + j) 
          and right_tile = tiles.(i * n + j + 1) in
          (* for the last line, check horizontal match only *)
          if (Tile.right_color tile <> Tile.left_color right_tile) then
            raise Out_Of_Loop
        done;
        true;
      end
    | _ -> begin
        for j = 0 to (n - 2) do
          let tile        = tiles.(i * n + j) 
          and right_tile  = tiles.(i * n + j + 1)
          and bottom_tile = tiles.((i + 1) * n + j) in
          (* check horizontal match *)
          if (Tile.right_color tile <> Tile.left_color right_tile) then
            raise Out_Of_Loop;
          (* check vertical match *)
          if (Tile.bottom_color tile <> Tile.top_color bottom_tile) then
            raise Out_Of_Loop;
        done;
        (* check the remaining lines recursively *)
        is_valid_line tiles (i + 1);
      end
    end in
    try
      is_valid_line tiles 0;
    with
    | Out_Of_Loop -> false
  end
  
  let generate m n c = begin
    let puzzle = make m n c in
    let tiles  = puzzle.tiles in
    (* generate random left colors for each tile *)
    let left_colors = Array.init (m * n) (fun _ -> Color.random c)
    (* generate random top colors for each tile *)
    and top_colors = Array.init (m * n) (fun _ -> Color.random c)
    (* then generate a well-formed solution from these colors *)
    in
    (* generate_line:
      function to generate each line of the puzzle recursively *)
    let rec generate_line tiles i = begin match (i = m - 1) with
    | true -> begin
        (* for the last line, the bottom colors are not constrained *)
        for j = 0 to n - 2 do
          let index = i * n + j in
          let right_c  = left_colors.(index + 1)
          and top_c    = top_colors.(index)
          and left_c   = left_colors.(index)
          and bottom_c = Color.random c in
          tiles.(index) <- Tile.make right_c top_c left_c bottom_c
        done;
        (* the rightmost tile's right color isn't constrained *)
        let index = i * n + (n - 1) in
        let right_c  = Color.random c
        and top_c    = top_colors.(index)
        and left_c   = left_colors.(index)
        and bottom_c = Color.random c in
        tiles.(index) <- Tile.make right_c top_c left_c bottom_c;
      end
    | _ -> begin
        (* here the bottom colors have to match the top colors of 
          the bottom neighbor *)
        for j = 0 to n - 2 do
          let index = i * n + j in
          let right_c  = left_colors.(index + 1)
          and top_c    = top_colors.(index)
          and left_c   = left_colors.(index)
          and bottom_c = top_colors.(index + n) in
          tiles.(index) <- Tile.make right_c top_c left_c bottom_c
        done;
        (* the rightmost tile's right color isn't constrained *)
        let index = i * n + (n - 1) in
        let right_c  = Color.random c
        and top_c    = top_colors.(index)
        and left_c   = left_colors.(index)
        and bottom_c = top_colors.(index + n) in
        tiles.(index) <- Tile.make right_c top_c left_c bottom_c;
        (* generate the remaining lines recursively *)
        generate_line tiles (i + 1);
      end
    end in
    generate_line tiles 0;
    (* shuffle the puzzle randomly 
      in order not to return the puzzle already solved *)
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
