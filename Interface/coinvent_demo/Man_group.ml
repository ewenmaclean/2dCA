
let r_open_in s = 
  let ic = open_in s in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  ignore(GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ());

  (* Create a label for the button and pack into box *)
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let options () =
  ()

let run () = 
  ()

let on_row_activated1 t c m n = 
  let arr=GTree.Path.get_indices m in
  let t = (snd (List.nth c arr.(0))) in 
  ()

let on_row_activated2 t c m n = 
  let arr=GTree.Path.get_indices m in
  let t = (snd (List.nth c arr.(0))) in 
  ()

let group_files path = 
    [("a",path^"a.txt");("b",path^"b.txt")] 
  

let manage path () = 
  let selection_changed (model:#GTree.model) selection = 
    let pr path = 
      let arr = GTree.Path.get_indices path in
      arr.(0)
    in
    pr (List.hd (selection#get_selected_rows))
  in
  let create_list_grp s choices () =
    let selection_changed (model:#GTree.model) selection = 
      let pr path = 
	let arr = GTree.Path.get_indices path in
	arr.(0)
      in
      pr (List.hd (selection#get_selected_rows))
    in
    let cols_lem = new GTree.column_list in
    let str_col_lem = cols_lem#add Gobject.Data.string in
  (* Create a new scrolled window, with scrollbars only if needed *)
    let scrolled_window = GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in    
    let model = GTree.list_store cols_lem in
    let treeview = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) () in
    model#clear ();
    List.iter
      (fun x -> let iter = model#append () in
		model#set ~row:iter ~column:str_col_lem (Printf.sprintf "%s" (fst x)))
      choices;
  (* get mutate results here *)
    let renderer = GTree.cell_renderer_text [] in
    let column = GTree.view_column ~title:"Input concepts"
      ~renderer:(renderer, ["text", str_col_lem]) () in
    treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
    ignore(treeview#selection#connect#changed ~callback:(fun () -> ignore(let x = (selection_changed model treeview#selection) in s#source_buffer#set_text (r_open_in (snd (List.nth choices x))))));
    ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated1 treeview choices m n));
    ignore(treeview#append_column column);
    scrolled_window#coerce 
  in
  let create_list_grp2 s choices () =
    let cols_lem = new GTree.column_list in
    let name_col = cols_lem#add Gobject.Data.string in
    let value_col = cols_lem#add Gobject.Data.string in
    let domain_col = cols_lem#add (Gobject.Data.gobject_by_name "GtkTreeModel") in
  (* Create a new scrolled window, with scrollbars only if needed *)
    let scrolled_window = GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
    let choices =  [("foo","a",["a";"b";"c"]);("bar","b",["a";"b";"c"])] in
    let name_renderer = GTree.cell_renderer_text [], ["text", name_col] in
    let string_col = (new GTree.column_list)#add Gobject.Data.string in
    let value_renderer = 
      GTree.cell_renderer_combo 
	[`VISIBLE true;`TEXT_COLUMN string_col;`HAS_ENTRY false;`EDITABLE true;] in
    let view_name_col = GTree.view_column ~title:"Concept" ~renderer:name_renderer () in
    let view_value_col = GTree.view_column ~title:"W. Scheme" ~renderer:(value_renderer,[]) () in
    let _ =
      view_value_col#add_attribute value_renderer "model" domain_col;
      view_value_col#add_attribute value_renderer "text" value_col;
    in
    let list_store = GTree.list_store cols_lem in
    let model = (list_store :> GTree.model) in
    let treeview = GTree.view ~model ~packing:(scrolled_window#add_with_viewport) ~show:true() in
    let _ = treeview#append_column view_name_col in
    let _ = treeview#append_column view_value_col in
    List.iter
      (fun (n,v,d) -> 
	(let iter = list_store#append ()  in
	 let d,_ = GTree.store_of_list Gobject.Data.string d in
	 list_store#set ~row:iter ~column:name_col (Printf.sprintf "%s" n);
	 list_store#set ~row:iter ~column:value_col v;
	 list_store#set ~row:iter ~column:domain_col (d#as_model)))
      choices;
    treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
    ignore(treeview#selection#connect#changed ~callback:(fun () -> ignore(let x = (selection_changed model treeview#selection) in s#source_buffer#set_text "some text")));
    ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated2 treeview [("a","a");("b","b")] m n));
    scrolled_window#coerce 
  in
  let windowmanage = GWindow.window ~title:"Blend Theory Group" 
    ~width:515 ~height:500 ~modal:true ~border_width:10 () in
  let vbox1 = GPack.vbox ~packing:windowmanage#add () in
  let vpm = GPack.paned `VERTICAL ~packing:vbox1#add () in
  let vbl = GPack.vbox ~packing:vpm#add1 () in
  let frame_man = GBin.frame ~packing:vbl#pack () in
  let vpain_man = GPack.paned `HORIZONTAL ~packing:vbl#add () in
  let bbox_man = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_man#add () in
  let button_options = GButton.button ~packing:bbox_man#add () in
  let button_run = GButton.button ~packing:bbox_man#add  () in
  let button_quit = GButton.button ~packing:bbox_man#add  ()  in
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Options" ~packing:button_options#add ());
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Run" ~packing:button_run#add ());
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Exit" ~packing:button_quit#add ());
  ignore(button_options#connect#clicked ~callback:options);
  ignore(button_run#connect#clicked ~callback:run);
  ignore(button_quit#connect#clicked ~callback:windowmanage#destroy);
  let scrolled_man = (GBin.scrolled_window
			~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
			()) in
  let source_man =
    (GSourceView2.source_view
       ~width:500 ~height:300
       ~packing:scrolled_man#add 
       ()) in
  vpm#add1 vbl#coerce;
  vpm#add2 scrolled_man#coerce;
  vpm#set_position 200;
  let l = create_list_grp source_man (group_files path) () in vpain_man#add1 l;
  let l = create_list_grp2 source_man [] () in vpain_man#add2 l;
  vpain_man#set_position 120;
  windowmanage#show ()