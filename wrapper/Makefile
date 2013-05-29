
test:vlcwrapper.cmxa
	ocamlopt test_vlc.ml -o test.native -cclib libvlcwrapper.a -cclib -lvlc -cclib -lvlccore 


ocamlwrapper.o:ocamlwrapper.c
	ocamlc -c $<

dllvlcwrapper.so:ocamlwrapper.o
	ocamlmklib -o vlcwrapper $< \
		-lvlc -lvlccore

vlcwrapper.mli:vlcwrapper.ml
	ocamlc -i $< > $@

vlcwrapper.cmi:vlcwrapper.mli
	ocamlc -c $<

vlcwrapper.cmo:vlcwrapper.ml vlcwrapper.cmi
	ocamlc -c $<

vlcwrapper.cma:vlcwrapper.cmo dllvlcwrapper.so
	ocamlc -a -o $@ $< -lvlc

vlcwrapper.cmx:vlcwrapper.ml vlcwrapper.cmi
	ocamlopt -c $<

vlcwrapper.cmxa:vlcwrapper.cmx dllvlcwrapper.so
	ocamlopt -a -o $@ $< \
		-cclib -lvlc -cclib -lvlccore

clean:
	rm -f *.[oa] *.so *.cm[ixoa] *.cmxa


