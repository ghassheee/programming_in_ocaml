LIBRARY=lib


all: main.ml 
	ocamlc -I . -I ${LIBRARY} -o fact10 lib/fact.ml lib/fuga.ml main.ml 


clean: 
	rm -rf ${LIBRARY}/*.cmo ${LIBRARY}/*.cmi *.cmi *.cmo
