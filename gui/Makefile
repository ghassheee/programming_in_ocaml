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
	${OCAML} eg_text.ml			-o eg_text
	${OCAML} eg_topmenu.ml		-o eg_topmenu
	${OCAML} eg_editor.ml		-o eg_editor
	${OCAML} eg_image_tag.ml	-o eg_image_tag
	${OCAML} eg_pack_bind.ml	-o eg_pack_bind
	${OCAML} eg_canvas.ml		-o eg_canvas
clean: 
	rm -rf bank
	rm -rf eg_top
	rm -rf eg_label
	rm -rf eg_frame
	rm -rf eg_textvariable
	rm -rf eg_entry
	rm -rf eg_message
	rm -rf eg_button
	rm -rf eg_buttonflash
	rm -rf eg_radiobutton
	rm -rf eg_checkbutton
	rm -rf eg_listbox
	rm -rf eg_scrollbar
	rm -rf eg_scale
	rm -rf eg_menu
	rm -rf eg_text
	rm -rf eg_topmenu
	rm -rf eg_editor
	rm -rf eg_image_tag
	rm -rf eg_pack_bind
	rm -rf eg_canvas
	rm *.cmi *.cmo 
