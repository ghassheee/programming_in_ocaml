
tablemain: tablemain.ml table.ml table.mli 
	ocamlc table.mli
	ocamlc -c ../base/base.ml -o base.cmo
	ocamlc -c table.ml
	ocamlc -c tablemain.ml
	ocamlc -o tablemain base.cmo table.cmo tablemain.cmo


clean: 
	rm *.cmi *.cmo

