(* init the seed for the pseudo-random generation of puzzle *)
Random.self_init;;
(* tile width and height (does not change) *)
let tile_width  = 60;;
let tile_height = 60;;
(* default values (subject to change) *)
let default_m   = 4;;
let default_n   = 4;;
let width       = ref 600;;
let height      = ref 600;;

(* init the models *)
let tile_set_model    = ref
  (Tetravex.Puzzle.generate default_m default_n 10);;
let puzzle_grid_model = ref
  (Tetravex.Puzzle.make default_m default_n 10);;
let puzzle_solution   = ref
  (Tetravex.Puzzle.make default_m default_n 10);;
  
(* init the views *)
let main_window      = ref (Tetravexview.Window.create !width !height);;
let puzzle_grid_view = ref (
  new Tetravexview.puzzle_grid 
    ~model:!puzzle_grid_model 
    ~width:(tile_width * default_n) 
    ~height:(tile_height * default_m)
    !main_window
  );;
let tile_set_view    = ref (
  new Tetravexview.tile_set 
    ~model:!tile_set_model
    ~width:(tile_width) 
    ~height:(tile_height) !main_window
  );;

(* is_dragging flag is true if the user pressed the mouse 
    and didn't release it yet *)
let is_dragging = ref false;;
(* drag_x, drag_y: 
   the mouse coordinates relative to the object being dragged *)
let drag_x      = ref 0;;
let drag_y      = ref 0;;

(* function triggered when the user clicks on a tile from the current set *)
let drag_start ~id:id ~model:model ~view:view ev = begin
  (* raise is_dragging flag and init the mouse coordinates *)
  is_dragging := true;
  drag_x := ev.Tk.ev_MouseX;
  drag_y := ev.Tk.ev_MouseY;
end

(* function triggered when the user drags (move) the tile *)
let drag_move ~id:id ~model:model ~view:view ev = if (!is_dragging) then begin
  (* compute the new position from the event's info *)
  let rootx = Winfo.x view
  and rooty = Winfo.y view in
  let x     = ev.Tk.ev_MouseX + rootx - !drag_x
  and y     = ev.Tk.ev_MouseY + rooty - !drag_y in
  (* update the tile position *)
  !tile_set_view#update_pos ~id:id ~x:x ~y:y ();
end

(* congratulations message *)
let msg = "    
    /@
    \\ \\
  ___> \\
 (__O)  \\
(____@)  \\
(____@)   \\
 (__o)_    \\
       \\    \\
CONGRATULATIONS!\n
";;

(* function called by drag_stop to handle a drop on the grid *)
let drop_receive ~id:id ~model:tile_model rootx rooty = begin
  (* compute the coordinates relative to the grid frame *)
  let x = rootx - Winfo.x (!puzzle_grid_view#get_frame)
  and y = rooty - Winfo.y (!puzzle_grid_view#get_frame) in
  (* try to locate a grid position from the given drop location *)
  try
    (* get the grid tile at the given drop location *)
    let (i, j) = !puzzle_grid_view#locate_tile x y in
    (* check if the given tile can be set at the given grid position *)
    if (Tetravex.Puzzle.can_set !puzzle_grid_model i j tile_model) then begin
      (* if it can be set, do it *)
      Tetravex.Puzzle.set !puzzle_grid_model i j tile_model;
      if (Tetravex.Puzzle.is_full !puzzle_grid_model) then begin
        (* if the puzzle was solved by the user, congratulate him *)
        Printf.printf "%s" msg;
        flush stdout;
      end;
      (* remove the dropped tile from the view *)
      !tile_set_view#remove ~id:id ();
      (* update the grid so that it displays the recieved tile *)
      !puzzle_grid_view#update ();
    end
  with
  | Tetravexview.No_Tile -> ();
end

(* function triggered when the user releases the click *)
let drag_stop ~id:id ~model:model ~view:view ev = begin
  (* withdraw is_dragging flag *)
  is_dragging := false;
  (* compute the drop location = 
  absolute coordinates of the center of the tile being dropped *)
  let rootx = Winfo.x view
  and rooty = Winfo.y view in
  let x     = tile_width / 2 + rootx
  and y     = tile_height / 2 + rooty in
  (* call the drop handler *)
  drop_receive ~id:id ~model:model x y;
end

(* function triggered when the user clicks on a tile from the puzzle grid *)
let drag_start_grid ~row:i ~col:j ~model:tile_model ~view:view ev =
  (* allow user to drag if the tile grid is not empty *)
  if not (Tetravex.Tile.is_empty tile_model) then begin
    (* get the tile absolute coordinates *)
    let x = Winfo.x view + Winfo.x !puzzle_grid_view#get_frame
    and y = Winfo.y view + Winfo.y !puzzle_grid_view#get_frame in
    Tetravex.Puzzle.set !puzzle_grid_model i j (Tetravex.Tile.empty ());
    let id = !tile_set_view#add ~model:tile_model ~x:x ~y:y () in
    !puzzle_grid_view#update ();
    !tile_set_view#update ();
    !tile_set_view#fire_drag_start ~id:id ev;
end

(* displays the current puzzle grid on screen *)
let display_grid () = begin
  Tk.pack [!puzzle_grid_view#get_frame] ~side:`Left ~padx:50;
  !puzzle_grid_view#update ();
end

(* displays the current set of tile on screen *)
let display_set ()  = begin
  !tile_set_view#update ();
end


let solving_thread = ref (Thread.self ());;

(* starts a new Tetravex puzzle with m rows and n columns *)
let new_game m n = begin
  (* create the models for the new game *)
  tile_set_model    := Tetravex.Puzzle.generate m n 10;
  puzzle_grid_model := Tetravex.Puzzle.make m n 10;
  (* destroy the old views *)
  !tile_set_view#destroy();
  !puzzle_grid_view#destroy();
  (* update the views corresponding to the new models *)
  puzzle_grid_view := new Tetravexview.puzzle_grid 
    ~model:!puzzle_grid_model
    ~width:tile_width ~height:tile_height
    !main_window;
  tile_set_view := new Tetravexview.tile_set 
    ~model:!tile_set_model 
    ~width:tile_width ~height:tile_height
    ~x:((!width / 2) + (!width / 2 - n * tile_height) / 2)
    ~y:((!height - (m * tile_height)) / 2)
    !main_window;
  (* set the drag and drop bindings to the new views *)
  !puzzle_grid_view#bind_drag_start drag_start_grid;
  !tile_set_view#bind_drag_start drag_start;
  !tile_set_view#bind_drag_move drag_move;
  !tile_set_view#bind_drag_stop drag_stop;
  (* display the new views *)
  display_grid ();
  display_set ();
  (* start to solve the puzzle in a separate thread *)
  puzzle_solution := Tetravex.Puzzle.copy !tile_set_model;
  solving_thread  := Thread.create Tetravex.Puzzle.solve !puzzle_solution;
end

(* displays the solution of the current puzzle on the grid
  (function called when the user clicks the "Get Solution" button) *)
let solve () = begin
  (* if it has not already terminated,
  wait for the termination of the thread that computes the solution *)
  Thread.join !solving_thread;
  (* fill the grid with the already computed solution *)
  Tetravex.Puzzle.fill !puzzle_grid_model !puzzle_solution;
  (* update the grid and destroy the set of tiles *)
  !puzzle_grid_view#update();
  !tile_set_view#destroy();
end
 
(* opens a puzzle from a file *)
let open_from_file path = begin
  (* load the models for the new game *)
  tile_set_model    := Tetravex.Puzzle.read path;
  let m              = Tetravex.Puzzle.height !tile_set_model
  and n              = Tetravex.Puzzle.width !tile_set_model in
  puzzle_grid_model := Tetravex.Puzzle.make m n 10;
  (* destroy the old views *)
  !tile_set_view#destroy();
  !puzzle_grid_view#destroy();
  (* update the views corresponding to the new models *)
  puzzle_grid_view := new Tetravexview.puzzle_grid
    ~model:!puzzle_grid_model
    ~width:tile_width ~height:tile_height
    !main_window;
  tile_set_view    := new Tetravexview.tile_set
    ~model:!tile_set_model 
    ~width:tile_width ~height:tile_height
    ~x:((!width / 2) + (!width / 2 - n * tile_height) / 2)
    ~y:((!height - (m * tile_height)) / 2)
    !main_window;
  (* set the drag and drop bindings to the new views *)
  !puzzle_grid_view#bind_drag_start drag_start_grid;
  !tile_set_view#bind_drag_start drag_start;
  !tile_set_view#bind_drag_move drag_move;
  !tile_set_view#bind_drag_stop drag_stop;
  (* display the new views *)
  display_grid ();
  display_set ();
  (* start to solve the new puzzle in separate thread *)
  puzzle_solution := Tetravex.Puzzle.copy !tile_set_model;
  solving_thread  := Thread.create Tetravex.Puzzle.solve !puzzle_solution;
end

(* saves the current puzzle *)
let save_as filename = begin
  Tetravex.Puzzle.save !tile_set_model filename;
end

(* creates the menu for the game *)
let make_menu () = Tetravexview.Menu.create
  ~width:!width
  ~newgamecallback:new_game ~solvecallback:solve
  ~opencallback:open_from_file ~savecallback:save_as
  ~rows:default_m ~columns:default_n
  !main_window
;;

(* displays the menu on screen *)
let display_menu menu = Tk.pack [menu] ~side:`Top ~fill:`X;;

(* starts the application *)
let start ?resolution:((w0, h0) = (800, 600)) () = begin
  (* init the resolution of the window *)
  width  := w0;
  height := h0;
  Tetravexview.Window.resize !main_window w0 h0;
  (* init the menu *)
  let menu = make_menu () in
  (* display the menu *)
  display_menu menu;
  (* start a new game *)
  new_game default_m default_n;
  (* display the puzzle grid *)
  display_grid ();
  (* display the set of tiles *)
  display_set ();
  (* start the application's main loop *)
  Printexc.print Tk.mainLoop ();
end
