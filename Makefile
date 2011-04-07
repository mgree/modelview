OCAMLOPT=ocamlopt unix.cmxa graphics.cmxa

ball : ball.cmx
	$(OCAMLOPT) $^ -o $@

world : world.cmx
	$(OCAMLOPT) $^ -o $@

%.cmx : %.ml
	$(OCAMLOPT) -c $<

clean :
	rm -f *.cmx *.cmi *.o ball world *~