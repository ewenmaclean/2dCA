let path_home = Unix.getenv "HOME"
let path = path_home^"/coinvent_demo/"
let locale = GtkMain.Main.init ()
let lang_file = path^"casl.lang"
let font_name = "Monospace 10"
let win = GWindow.window ~allow_grow:true ~allow_shrink:true ~border_width:10 ~width:1000 ~height:800 ~title:"CoInvent" ()
let vbox1 = GPack.vbox ~packing:win#add ()

let clipboard = GData.clipboard Gdk.Atom.clipboard
let strip_spaces buf = 
  String.trim buf
let r_open_in s = 
  let ic = open_in s in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf
let nothing () = ()


let run2dca () = 
  ignore(Unix.system(path^"2dca.jar&"));()
(* PARSE FOR MUSIC OR MATHEMATICS HERE - CALL PDF *)



let get_text_from_selected () = 
  let buf2 = clipboard#text  in
  let buf = 
  (match buf2 with
    | None -> ""
    | Some x -> x) 
  in
  let buf3 = strip_spaces buf  in
  if (buf3 = "") then () else
  (
   (* CHECK FOR MUSIC ETC *)
    (* PARSE *)
    let lex = Lexing.from_string buf3 in
    let music = Musicparser.chord1 Musiclexer.token lex in
    let musictex = 
    (match Chord.findroot music 
      with
      | None ->
(*needs asking here*)
	  Chord.calcnotesfromlist 1 (Chord.calcnoteslist 1 music)
      | Some x ->
          Chord.calcnotesfromlist x (Chord.calcnoteslist x music))
    in
    let pre = r_open_in (path^"render_music_pre.tex") in
    let post = r_open_in (path^"render_music_post.tex") in
    let musictexfull = pre^"\n"^musictex^"\n"^post^"\n" in
    let oc = open_out (path^"render.tex") in
    Printf.fprintf oc "%s" musictexfull;
    close_out oc;
    (* SEND COMMANDS -- USE TRY HERE *)
    ignore(Unix.system("pdflatex -output-directory "^path^" "^path^"render.tex"));
    ignore(Unix.system("cpdf -scale-page \"5 5\" "^path^"render.pdf -o render2.pdf"));
    ignore(Unix.system("convert -trim "^path^"render2.pdf "^path^"render.jpg")); 
  
   let w = GWindow.dialog ~modal:true ~height:500 ~width:700 () in
   let _ = GMisc.image ~file:(path^"render.jpg") ~packing: w#vbox#add () in
  w#show ())

  

let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  ignore(GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ());

  (* Create a label for the button and pack into box *)
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let vpain_all = GPack.paned `VERTICAL ~packing:vbox1#add ()
let vblt = GPack.vbox ~packing:vpain_all#add1 ()
let menu_bar = GMenu.menu_bar ~packing:vblt#pack ()
let frame_main = GBin.frame ~packing:vblt#pack () 


let bbox_main = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_main#add () 



let button_open1 = GButton.button ~packing:bbox_main#add  () 
let button_open2 = GButton.button ~packing:bbox_main#add  ()
let button_blend = GButton.button ~packing:bbox_main#add  ()
let button_render = GButton.button ~packing:bbox_main#add  ()  
let button_quit = GButton.button ~packing:bbox_main#add  ()  

let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Openl" ~packing:button_open1#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Openr" ~packing:button_open2#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Mutate" ~packing:button_blend#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Render" ~packing:button_render#add ()
let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Quit" ~packing:button_quit#add ()



let file_item = GMenu.menu_item ~label:"File" ~packing:menu_bar#append()
let options_item = GMenu.menu_item ~label:"Options" ~packing:menu_bar#append()
let theory_item = GMenu.menu_item ~label:"Theory" ~packing:menu_bar#append()
let action_item = GMenu.menu_item ~label:"Action" ~packing:menu_bar#append()


let vpain_lem_thm = GPack.paned `HORIZONTAL ~packing:vblt#add ()

let scrolled_win1 = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vpain_lem_thm#add1
     ())

let source_view1 =
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_win1#add 
    ())

let _ = ignore(source_view1#source_buffer#connect#changed ~callback:(fun () -> source_view1#source_buffer#add_selection_clipboard clipboard))
 
let l = GSourceView2.source_language_manager ~default:true

let lang = l#guess_language ~content_type:"text/casl" () 

let _ = 
  match lang with
  | None -> print_endline ("None: "^lang_file)
  | _ -> print_endline "Some"


let _ = ignore(source_view1#source_buffer#set_language lang)

let scrolled_win2 = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vpain_lem_thm#add2
     ())

let source_view2 =
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_win2#add 
    ())

let _ = ignore(source_view2#source_buffer#connect#changed ~callback:(fun () -> source_view2#source_buffer#add_selection_clipboard clipboard))

    
let selection_changed (model:#GTree.model) selection = 
  let pr path = 
    let arr = GTree.Path.get_indices path in
    arr.(0)
  in
  pr (List.hd (selection#get_selected_rows))
    
let show_new_window t () = 
  let win2 = GWindow.window ~allow_grow:true ~allow_shrink:true ~border_width:10 ~modal:true ~width:800 ~height:600 ~title:"CoInvent" () in
  let vbox2 = GPack.vbox ~packing:win2#add () in
  let vp = GPack.paned `VERTICAL ~packing:vbox2#add () in
  let vb = GPack.vbox ~packing:vp#add1 () in
  let frame_sub = GBin.frame ~packing:vb#pack () in
  let bbox_sub = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_sub#add () in
  let button_rendersub = GButton.button ~packing:bbox_sub#add  () in
  let button_add = GButton.button ~packing:bbox_sub#add  () in
  let button_exit = GButton.button ~packing:bbox_sub#add  () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Render" ~packing:button_rendersub#add () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Export" ~packing:button_add#add () in
  let _ = xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Exit" ~packing:button_exit#add () in
  let scrolled_winnew = (GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ~packing:vp#add2
     ())
  in
  let source_viewnew =
  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:scrolled_winnew#add 
    ()) in
  let _ = ignore(source_viewnew#source_buffer#connect#changed ~callback:(fun () -> source_viewnew#source_buffer#add_selection_clipboard clipboard))
  in source_viewnew#buffer#set_text t;
  let _ = button_rendersub#connect#clicked ~callback:(get_text_from_selected) in
  let _ = button_add#connect#clicked ~callback:(nothing) in
  let _ =button_exit#connect#clicked ~callback:(win2#destroy) in
  win2#show ()

let on_row_activated t c m n = 
  let arr=GTree.Path.get_indices m in
  let t = (snd (List.nth c arr.(0))) in 
  show_new_window t ()


let create_list_blends choices () =
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
  let column = GTree.view_column ~title:"Computed Blends"
    ~renderer:(renderer, ["text", str_col_lem]) () in
  treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
  ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated treeview choices m n));
  ignore(treeview#append_column column);
  scrolled_window#coerce
    
let add_blends () =
(*%%% need to call amalgams here*)
  

  let list = create_list_blends [("a","A_TEXT");("b","B_TEXT");("c","C_TEXT")] () in 
  vpain_all#add2 list

let r_open_in s = 
  let ic = open_in s in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let file_ok_sel s filew () = 
  s#source_buffer#set_text "";
  let buf = r_open_in filew#filename in
  s#source_buffer#set_text buf;
  filew#destroy()
  

let file_open_m s () = 
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10 () in
  let _ = filew#ok_button#connect#clicked ~callback:(file_ok_sel s filew) in
  let _ = filew#cancel_button#connect#clicked ~callback:(filew#destroy) in
  filew#show ()

let open1_file () =
   file_open_m source_view1 ()

let open2_file () =
   file_open_m source_view2 ()

let create_menu_file ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Openl" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(open1_file));
  let item = GMenu.menu_item ~label: "Openr" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(open2_file));
  let item = GMenu.menu_item ~label: "Exit" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(GMain.Main.quit))

let create_menu_options ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Search Depth" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing);
  let item = GMenu.menu_item ~label: "Generic Space Limit" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing);
  let item = GMenu.menu_item ~label: "Generic Space Calculator" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing)


let create_menu_theory ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Music" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing);
  let item = GMenu.menu_item ~label: "Mathematics" ~packing:file_menu#append () in ignore (item#connect#activate ~callback:nothing);
  let item = GMenu.menu_item ~label: "Cellular Automata" ~packing:file_menu#append () in ignore (item#connect#activate ~callback:run2dca)

let create_menu_actions ~packing () = 
  let file_menu = GMenu.menu ~packing () in
  let item = GMenu.menu_item ~label: "Compute Blends" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:(add_blends));
  let item = GMenu.menu_item ~label: "Render Selection" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:get_text_from_selected);
  let item = GMenu.menu_item ~label: "Manage Theory Group" ~packing:file_menu#append () in
    ignore (item#connect#activate ~callback:nothing)



let _ = button_open1#connect#clicked ~callback:(open1_file)
let _ = button_open2#connect#clicked ~callback:(open2_file)
let _ = button_blend#connect#clicked ~callback:(add_blends)
let _ = button_render#connect#clicked ~callback:(get_text_from_selected)
let _ = button_quit#connect#clicked ~callback:(GMain.Main.quit)



let _ = 
  vpain_lem_thm#set_position 450;
  vpain_all#set_position 550;  
  ignore (win#connect#destroy (fun _ -> GMain.quit ()));
  create_menu_file ~packing:file_item#set_submenu ();
  create_menu_options ~packing:options_item#set_submenu ();
  create_menu_theory ~packing:theory_item#set_submenu ();
  create_menu_actions ~packing:action_item#set_submenu ();
  win#show ();
  GMain.Main.main ()
