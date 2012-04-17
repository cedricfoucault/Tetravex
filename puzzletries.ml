(* let solve problem = begin
  let m = problem.m and n = problem.n and tiles = problem.tiles in
  let mn = m * n in
  (* Finite domain: *)
  (* Each tile must be placed in one of the m * n position on the board *)
  let lines = Fd.array mn 0 (m - 1)
  and columns = Fd.array mn 0 (n - 1) in
  let get_position i j = Arith.e2fd ((fd2e i *~ i2e n) +~ fd2e j) in
  (* let positions = Fd.array mn 0 (mn - 1) in *)
  let positions = Array.make mn lines.(0) in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      positions.(i * n + j) <- get_position lines.(i) columns.(j)
    done
  done;
  for pos = 0 to mn - 1 do
    positions.(pos) <- get_position lines.(pos) columns.(pos)
  done;
  (* Global constraint: *)
  (* No two tiles are at the same position *)
  Cstr.post (Alldiff.cstr positions);
  (* Local constraints: *)
  (* Two tiles whose color don't match can't be next to each other *)
  for pos1 = 0 to mn - 1 do
    for pos2 = 0 to mn - 1 do
      if (pos1 <> pos2) then begin
        let tile1 = nth_tile tiles pos1 and tile2 = nth_tile tiles pos2 in
        if (right_color tile1 <> left_color tile2) then
          (* position of tile1 <> position of tile2 - 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) -~ i2e 1))
          (* column of tile1 = n - 1 (tile1 on right border), or *)
          and c2 = (fd2e columns.(pos1) =~ i2e (n - 1))
          (* column of tile2 = 0 (tile2 on left border) *)
          and c3 = (fd2e columns.(pos2) =~ i2e 0)
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        (* if left color of tile1 and right color of tile2 don't match  *)
        if (left_color tile1 <> right_color tile2) then
          (* position of tile1 <> position of tile2 + 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) +~ i2e 1))
          (* column of tile1 = 0 (tile1 on left border) *)
          and c2 = (fd2e columns.(pos1) =~ i2e 0)
          (* column of tile2 = 0 (tile2 on left border) *)
          and c3 = (fd2e columns.(pos2) =~ i2e (n - 1))
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (top_color tile1 <> bottom_color tile2) then
          (* line of tile1 <> line of tile2 + 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) +~ i2e n))
          (* line of tile1 = 0 (tile1 on top border), or *)
          and c2 = (fd2e lines.(pos1) =~ i2e 0)
          (* line of tile2 = m - 1 (tile2 on bottom border) *)
          and c3 = (fd2e lines.(pos2) =~ i2e (m - 1))
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (bottom_color tile1 <> top_color tile2) then
          (* line of tile1 <> line of tile2 - 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) -~ i2e n))
          (* line of tile1 = m - 1 (tile1 on bottom border), or *)
          and c2 = (fd2e lines.(pos1) =~ i2e (m - 1))
          (* line of tile2 = 0 (tile2 on top border) *)
          and c3 = (fd2e lines.(pos2) =~ i2e 0)
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
      end
    done
  done;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  else raise No_Solution
end;; *)

(* let solve problem = begin
  let m = problem.m and n = problem.n and tiles = problem.tiles in
  let mn = m * n in
  (* Finite domain: *)
  (* Each tile must be placed in one of the m * n position on the board *)
  let positions = Fd.array mn 0 (mn - 1) in
  (* Global constraint: *)
  (* No two tiles are at the same position *)
  Cstr.post (Alldiff.cstr positions);
  (* Local constraints: *)
  (* Two tiles whose color don't match can't be next to each other *)
  for i1 = 0 to (mn - 1) do
    for i2 = 0 to (mn - 1) do
      if i1 <> i2 then begin
        let tile1 = nth_tile tiles i1 and tile2 = nth_tile tiles i2 in
        (* if right color of tile1 and left color of tile2 don't match *)
        if (right_color tile1 <> left_color tile2) then
          (* position of tile1 <> position of tile2 - 1, or *)
          let c1 = (fd2e positions.(i1) <>~ (fd2e positions.(i2) -~ i2e 1))
          (* position of tile1 % n = n - 1 (tile1 on right border), or *)
          and c2 = ((fd2e positions.(i1) %~ i2e n) =~ i2e (n - 1))
          (* and c2 = ((((fd2e positions.(i1) +~ i2e 1) /~ i2e n) *~ i2e n) =~ (fd2e positions.(i1) +~ i2e n)) *)
          (* position of tile2 % n = 0 (tile2 on left border) *)
          and c3 = ((fd2e positions.(i2) %~ i2e n) =~ i2e 0)
          (* and c3 = (((fd2e positions.(i2) /~ i2e n) *~ i2e n) =~ fd2e positions.(i2)) *)
          in
          Cstr.post (c1 ||~~ c2 ||~~ c3);
        (* if left color of tile1 and right color of tile2 don't match  *)
        if (left_color tile1 <> right_color tile2) then
          (* position of tile1 <> position of tile2 + 1, or *)
          let c1 = (fd2e positions.(i1) <>~ (fd2e positions.(i2) +~ i2e 1))
          (* position of tile1 % n = 0 (tile1 on left border) *)
          and c2 = ((fd2e positions.(i1) %~ i2e n) =~ i2e 0)
          (* and c2 = (((fd2e positions.(i1) /~ i2e n) *~ i2e n) =~ fd2e positions.(i1)) *)
          (* position of tile2 % n = (n - 1) (tile2 on right border) *)
          and c3 = ((fd2e positions.(i2) %~ i2e n) =~ i2e (n - 1))
          (* and c3 = ((((fd2e positions.(i2) +~ i2e 1) /~ i2e n) *~ i2e n) =~ (fd2e positions.(i2) +~ i2e n)) *)
          in
          Cstr.post (c1 ||~~ c2 ||~~ c3);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (top_color tile1 <> bottom_color tile2) then
          (* position of tile1 <> position of tile2 + n, or *)
          let c1 = (fd2e positions.(i1) <>~ (fd2e positions.(i2) +~ i2e n))
          (* position of tile1 < n (tile1 on top border), or *)
          and c2 = (fd2e positions.(i1) <~ i2e n)
          (* position of tile2 > m * (n - 1) (tile2 on bottom border) *)
          and c3 = (fd2e positions.(i2) >=~ i2e (m * (n - 1)))
          in
          Cstr.post (c1 ||~~ c2 ||~~ c3);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (bottom_color tile1 <> top_color tile2) then
          (* position of tile1 <> position of tile2 - n, or *)
          let c1 = (fd2e positions.(i1) <>~ (fd2e positions.(i2) -~ i2e n))
          (* position of tile1 > n * (n - 1) (tile1 on bottom border), or *)
          and c2 = (fd2e positions.(i1) >=~ i2e (m * (n - 1)))
          (* position of tile2 < n (tile2 on top border) *)
          and c3 = (fd2e positions.(i2) >~ i2e (m * (n - 1)))
          in
          Cstr.post (c1 ||~~ c2 ||~~ c3);
      end;
    done;
  done;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then begin
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  end 
  else raise No_Solution
end;; *)

let solve problem = begin
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
    for pos2 = 0 to mn - 1 do
      if (pos1 <> pos2) then begin
        let tile1 = nth_tile tiles pos1 and tile2 = nth_tile tiles pos2 in
        if (right_color tile1 <> left_color tile2) then begin
          (* position of tile1 <> position of tile2 - 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) -~ i2e 1))
          (* column of tile1 = n - 1 (tile1 on right border), or *)
          and c2 = (fd2e columns.(pos1) =~ i2e (n - 1))
          (* column of tile2 = 0 (tile2 on left border) *)
          and c3 = (fd2e columns.(pos2) =~ i2e 0)
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        end;
        (* if left color of tile1 and right color of tile2 don't match  *)
        if (left_color tile1 <> right_color tile2) then
          (* position of tile1 <> position of tile2 + 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) +~ i2e 1))
          (* column of tile1 = 0 (tile1 on left border) *)
          and c2 = (fd2e columns.(pos1) =~ i2e 0)
          (* column of tile2 = 0 (tile2 on left border) *)
          and c3 = (fd2e columns.(pos2) =~ i2e (n - 1))
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (top_color tile1 <> bottom_color tile2) then
          (* line of tile1 <> line of tile2 + 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) +~ i2e n))
          (* line of tile1 = 0 (tile1 on top border), or *)
          and c2 = (fd2e lines.(pos1) =~ i2e 0)
          (* line of tile2 = m - 1 (tile2 on bottom border) *)
          and c3 = (fd2e lines.(pos2) =~ i2e (m - 1))
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
        (* if top color of tile1 and bottom color of tile2 don't match  *)
        if (bottom_color tile1 <> top_color tile2) then
          (* line of tile1 <> line of tile2 - 1, or *)
          let c1 = (fd2e positions.(pos1) <>~ (fd2e positions.(pos2) -~ i2e n))
          (* line of tile1 = m - 1 (tile1 on bottom border), or *)
          and c2 = (fd2e lines.(pos1) =~ i2e (m - 1))
          (* line of tile2 = 0 (tile2 on top border) *)
          and c3 = (fd2e lines.(pos2) =~ i2e 0)
          in
          Cstr.post (c3 ||~~ c2 ||~~ c1);
      end
    done
  done;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then begin
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  end 
  else raise No_Solution
end;;

let solve problem = begin
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
  let cr = Array.init mn (fun i -> (fd2e columns.(i) =~ i2e (n - 1)))
  and cl = Array.init mn (fun i -> (fd2e columns.(i) =~ i2e 0))
  and cu = Array.init mn (fun i -> (fd2e lines.(i) =~ i2e 0))
  and cb = Array.init mn (fun i -> (fd2e lines.(i) =~ i2e (m - 1)))
  for pos1 = 0 to mn - 1 do
    (* right neighbor constraint *)
    let cr = ref (fd2e columns.(pos1) =~ (i2e (n - 1)))
    (* left neighbor constraint *)
    and cl = ref (fd2e columns.(pos1) =~ (i2e 0))
    (* up neighbor constraint *)
    and cu = ref (fd2e lines.(pos1) =~ (i2e 0))
    (* bottom neighbor constraint *)
    and cb = ref (fd2e lines.(pos1) =~ (i2e (m - 1)))
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
        (* if left color of tile1 and right color of tile2 match  *)
        if (left_color tile1 = right_color tile2) then begin
          (* position of tile1 = position of tile2 + 1 *)
          cl := !cl ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e 1));
        end;
        (* if top color of tile1 and bottom color of tile2 match  *)
        if (top_color tile1 = bottom_color tile2) then begin
          (* line of tile1 = line of tile2 + 1 *)
          cu := !cu ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e n));
        end;
        (* if bottom color of tile1 and top color of tile2 match  *)
        if (bottom_color tile1 = top_color tile2) then begin
          (* line of tile1 = line of tile2 - 1 *)
          cb := !cb ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e n));
        end;
      end;
    done;
    Cstr.post !cr;
    Cstr.post !cl;
    Cstr.post !cu;
    Cstr.post !cb;
  done;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then begin
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  end 
  else raise No_Solution
end;;

let solve problem = begin
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
  let cr = Array.init mn (fun i -> (fd2e columns.(i) =~ i2e (n - 1), 1))
  and cl = Array.init mn (fun i -> (fd2e columns.(i) =~ i2e 0), 1)
  and cu = Array.init mn (fun i -> (fd2e lines.(i) =~ i2e 0), 1)
  and cb = Array.init mn (fun i -> (fd2e lines.(i) =~ i2e (m - 1), 1))
  in
  for pos1 = 0 to mn - 1 do
    for pos2 = 0 to mn - 1 do
      if (pos1 <> pos2) then begin
        let tile1 = nth_tile tiles pos1 and tile2 = nth_tile tiles pos2 in
        (* if right color of tile1 and left color of tile2 match *)
        if (right_color tile1 = left_color tile2) then begin
          (* position of tile1 = position of tile2 - 1 *)
          cr.(pos1) <- ((fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e 1)) ||~~ fst cr.(pos1), snd cr.(pos1) + 1);
          (* cr.(pos1) <- cr.(pos1) ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e 1)); *)
        end;
        (* if left color of tile1 and right color of tile2 match  *)
        if (left_color tile1 = right_color tile2) then begin
          (* position of tile1 = position of tile2 + 1 *)
          cl.(pos1) <- ((fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e 1)) ||~~ fst cl.(pos1), snd cl.(pos1) + 1);
          (* cl.(pos1) <- cl.(pos1) ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e 1)); *)
        end;
        (* if top color of tile1 and bottom color of tile2 match  *)
        if (top_color tile1 = bottom_color tile2) then begin
          (* line of tile1 = line of tile2 + 1 *)
          cu.(pos1) <- ((fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e n)) ||~~ fst cu.(pos1), snd cu.(pos1) + 1);
          (* cu.(pos1) <- cu.(pos1) ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) +~ i2e n)); *)
        end;
        (* if bottom color of tile1 and top color of tile2 match  *)
        if (bottom_color tile1 = top_color tile2) then begin
          (* line of tile1 = line of tile2 - 1 *)
          cb.(pos1) <- ((fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e n)) ||~~ fst cb.(pos1), snd cb.(pos1) + 1);
          (* cb.(pos1) <- cb.(pos1) ||~~
            (fd2e positions.(pos1) =~ (fd2e positions.(pos2) -~ i2e n)); *)
        end;
      end;
    done;
  done;
  Array.sort (fun c1 -> fun c2 -> snd c1 - snd c2) cr;
  Array.iter (fun c -> Cstr.post (fst c)) cr;
  Array.sort (fun c1 -> fun c2 -> snd c1 - snd c2) cl;
  Array.iter (fun c -> Cstr.post (fst c)) cl;
  Array.sort (fun c1 -> fun c2 -> snd c1 - snd c2) cu;
  Array.iter (fun c -> Cstr.post (fst c)) cu;
  Array.sort (fun c1 -> fun c2 -> snd c1 - snd c2) cb;
  Array.iter (fun c -> Cstr.post (fst c)) cb;
  (* Try to label every position by constraint solving *)
  if Goals.solve (Goals.Array.labeling positions) then begin
    (* if a solution has been found, return the array of positions *)
    Array.map Fd.elt_value positions
  end 
  else raise No_Solution
end;;
