BASE=ocamlc -c ../base/base.ml -o base.cmo 
OCAML=ocamlfind ocamlc -linkpkg -package labltk labltk.cma unix.cma base.cmo
all: 
	${BASE}
	${OCAML} bank.ml			-o bank
	${OCAML} eg_top.ml			-o eg_top
	${OCAML} eg_label.ml		-o eg_label
	${OCAML} eg_frame.ml		-o eg_frame
	${OCAML} eg_textvariable.ml	-o eg_textvariable
	${OCAML} eg_entry.ml		-o eg_entry
	${OCAML} eg_message.ml		-o eg_message
	${OCAML} eg_button.ml		-o eg_button
	${OCAML} eg_buttonflash.ml 	-o eg_buttonflash
	${OCAML} eg_radiobutton.ml	-o eg_radiobutton
	${OCAML} eg_checkbutton.ml	-o eg_checkbutton
	${OCAML} eg_listbox.ml 		-o eg_listbox
	${OCAML} eg_scrollbar.ml	-o eg_scrollbar
	${OCAML} eg_scale.ml		-o eg_scale
	${OCAML} eg_menu.ml			-o eg_menu
	${OCAML} eg_topmenu.ml		-o eg_topmenu
clean: 
	rm *.cmi *.cmo 
