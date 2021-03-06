let win = GWindow.window ~show:true () in
let cols = new GTree.column_list in
let colnew = new GTree.column_list in
let s_col = colnew#add Gobject.Data.string in
let name_col = cols#add Gobject.Data.string in
let value_col = cols#add Gobject.Data.string in
let domain_col = cols#add ((GTree.list_store colnew):>(Gobject.data_conv)) in
let name_renderer = GTree.cell_renderer_text [], ["text", name_col] in
let string_col = (new GTree.column_list)#add Gobject.Data.string in
let value_renderer =
  GTree.cell_renderer_combo
    [ `VISIBLE true;
      `TEXT_COLUMN string_col;
      `HAS_ENTRY false;
      `EDITABLE true; ]
in
let view_name_col = GTree.view_column ~renderer:name_renderer () in
let view_value_col = GTree.view_column ~renderer:(value_renderer, []) () in
let _ =
  view_value_col#add_attribute value_renderer "model" domain_col;
  view_value_col#add_attribute value_renderer "text" value_col;
in
let choices = [("foo","0",["0";"1";"2"]);
	       ("bar","1",["1";"2";"3"]);
	       ("baz","2", ["2";"3";"4"])] 
in
let list_store = GTree.list_store cols in
let model = (list_store :> GTree.model) in

let view = GTree.view ~model ~packing:win#add ~show:true () in
let _ = view#append_column view_name_col in
let _ = view#append_column view_value_col in

let change_val i p (ls:GTree.list_store) = 
  let iter = model#get_iter p in
  let d = model#get ~row:iter ~column:domain_col in   
  let m = (d :> GTree.model) in
  ()
in
ignore(value_renderer#connect#changed ~callback:(fun tp iter -> change_val iter tp list_store));

let append (list_store: GTree.list_store) n v d =
  let d, _ = GTree.store_of_list Gobject.Data.string d in
  let iter = list_store#append () in
  list_store#set ~row:iter ~column:name_col n;
  list_store#set ~row:iter ~column:value_col v;
  list_store#set ~row:iter ~column:domain_col d#as_model;
in
List.iter (fun (x,y,z) -> append list_store x y z) choices;

GMain.Main.main ()
