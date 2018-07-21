TARGET 	= ilogic
LABLCMA = labltk.cma
STRCMA 	= str.cma
INCLUDE = -I +labltk
OBJS	= mySupport.cmo board.cmo input.cmo gui.cmo
OCAMLC 	= ocamlfind ocamlc -package labltk

all: $(DEPEND) $(TARGET)

ilogic: $(OBJS)
	$(OCAMLC) $(INCLUDE) -o $(TARGET) $(LABLCMA) $(STRCMA) $(OBJS)

%.cmi: %.mli
	$(OCAMLC) -c $(INCLUDE) $<

%.cmo:  %.ml
	$(OCAMLC) -c $(INCLUDE) $<

clean::
	-rm -rf *.cmi *.cmo $(TARGET) *.cmx *~

depend:: $(DEPEND)
	ocamldep *.mli *.ml > .depend

-include .depend



