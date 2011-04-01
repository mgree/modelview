OCAMLOPT=ocamlopt unix.cmxa graphics.cmxa

MLFILES=ball.ml
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))

ball : $(CMXFILES)
	$(OCAMLOPT) $^ -o $@

test : ball
	./ball

%.cmx : %.ml
	$(OCAMLOPT) -c $<

clean :
	rm -f *.cmx *.cmi *.o ball *~