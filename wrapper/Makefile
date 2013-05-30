OCAMLC=ocamlc -thread unix.cma
OCAMLOPT=ocamlopt -g -thread unix.cmx

fft:vlcwrapper.cmx vlcwrapper.cmxa th.cmx seq.cmx run-fft.cmx fft.cmx fft.cmi kahn.cmx kahn.cmi lockedtable.cmx semaphore.cmx
	ocamlopt -thread unix.cmxa threads.cmxa lockedtable.cmx semaphore.cmx kahn.cmx fft.cmx th.cmx seq.cmx run-fft.cmx -o run.native -cclib libvlcwrapper.a -cclib -lvlc -cclib -lvlccore

test:vlcwrapper.cmxa
	ocamlopt test_vlc.ml -o test.native -cclib libvlcwrapper.a -cclib -lvlc -cclib -lvlccore 

ocamlwrapper.o:ocamlwrapper.c
	$(OCAMLC) -c $<

dllvlcwrapper.so:ocamlwrapper.o
	ocamlmklib -o vlcwrapper $< \
		-lvlc -lvlccore

vlcwrapper.cmi:vlcwrapper.mli
	$(OCAMLC) -c $<

vlcwrapper.mli:vlcwrapper.ml
	$(OCAMLC) -i $< > $@

vlcwrapper.cmo:vlcwrapper.ml vlcwrapper.ml vlcwrapper.cmi
	$(OCAMLC) -c $<

vlcwrapper.cma:vlcwrapper.cmo dllvlcwrapper.so
	$(OCAMLC) -a -o $@ $< -lvlc

vlcwrapper.cmx:vlcwrapper.ml vlcwrapper.cmi
	ocamlopt -c $<

vlcwrapper.cmxa:vlcwrapper.cmx dllvlcwrapper.so
	ocamlopt -a -o $@ $< \
		-cclib -lvlc -cclib -lvlccore

clean:
	rm -f *.[oa] *.so *.cm[ixoa] *.cmxa


example.cmo:example.ml example.ml kahn.cmo interface.cmi
	$(OCAMLC) -c $<

example.cmx:example.ml kahn.cmx interface.cmi
	$(OCAMLOPT) -c $<

example.cmi:example.mli
	$(OCAMLC) -c $<

interface.cmi:interface.mli
	$(OCAMLC) -c $<

example.mli:example.ml
	$(OCAMLC) -i $< > $@

fft.cmo:fft.ml kahn.cmo kahn.cmi interface.cmi
	$(OCAMLC) -c $<

fft.cmx:fft.ml kahn.cmx fft.cmi kahn.cmi interface.cmi
	$(OCAMLOPT) -c $<

fft.cmi:fft.mli
	$(OCAMLC) -c $<

fft.mli:fft.ml kahn.cmi
	$(OCAMLC) -i $< > $@

kahn.cmo:kahn.ml semaphore.cmo lockedtable.cmo interface.cmi
	$(OCAMLC) -c $<

kahn.cmx:kahn.ml kahn.cmi semaphore.cmi lockedtable.cmi interface.cmi
	echo kahn
	$(OCAMLOPT) -c $<

kahn.cmi:kahn.mli
	$(OCAMLC) -c $<

kahn.mli:kahn.ml interface.cmi lockedtable.cmi semaphore.cmi
	$(OCAMLC) -i $< > $@

lockedtable.cmo:lockedtable.ml 
	$(OCAMLC) -c $<

lockedtable.cmx:lockedtable.ml 
	$(OCAMLOPT) -c $< 

lockedtable.cmi:lockedtable.mli
	$(OCAMLC) -c $<

lockedtable.mli:lockedtable.ml
	$(OCAMLC) -i $< > $@

net.cmo:net.ml semaphore.cmo lockedtable.cmo
	$(OCAMLC) -c $<

net.cmx:net.ml semaphore.cmx lockedtable.cmx
	$(OCAMLOPT) -c $<

net.cmi:net.mli
	$(OCAMLC) -c $<

net.mli:net.ml
	$(OCAMLC) -i $< > $@

pr.cmo:pr.ml 
	$(OCAMLC) -c $<

pr.cmx:pr.ml 
	$(OCAMLOPT) -c $<

pr.cmi:pr.mli
	$(OCAMLC) -c $<

pr.mli:pr.ml
	$(OCAMLC) -i $< > $@

re.cmo:re.ml 
	$(OCAMLC) -c $<

re.cmx:re.ml 
	$(OCAMLOPT) -c $<

re.cmi:re.mli
	$(OCAMLC) -c $<

re.mli:re.ml
	$(OCAMLC) -i $< > $@

run-fft.cmo:run-fft.ml vlcwrapper.cmi th.cmo fft.cmo fft.cmi
	$(OCAMLC) -c $<

run-fft.cmx:run-fft.ml vlcwrapper.cmx th.cmx fft.cmx fft.cmi
	$(OCAMLOPT) -c $<

run-fft.cmi:run-fft.mli
	$(OCAMLC) -c $<

run-fft.mli:run-fft.ml
	$(OCAMLC) -i $< > $@

run-pr.cmo:run-pr.ml pr.cmo example.cmo
	$(OCAMLC) -c $<

run-pr.cmx:run-pr.ml pr.cmx example.cmx
	$(OCAMLOPT) -c $<

run-pr.cmi:run-pr.mli
	$(OCAMLC) -c $<

run-pr.mli:run-pr.ml
	$(OCAMLC) -i $< > $@

run-seq.cmo:run-seq.ml seq.cmo example.cmo
	$(OCAMLC) -c $<

run-seq.cmx:run-seq.ml seq.cmx example.cmx
	$(OCAMLOPT) -c $<

run-seq.cmi:run-seq.mli
	$(OCAMLC) -c $<

run-seq.mli:run-seq.ml
	$(OCAMLC) -i $< > $@

run-socket-1.cmo:run-socket-1.ml serveur.cmo example.cmo
	$(OCAMLC) -c $<

run-socket-1.cmx:run-socket-1.ml serveur.cmx example.cmx
	$(OCAMLOPT) -c $<

run-socket-1.mli:run-socket-1.ml
	$(OCAMLC) -i $< > $@

run-socket-2.cmo:run-socket-2.ml net.cmo example.cmo
	$(OCAMLC) -c $<

run-socket-2.cmx:run-socket-2.ml net.cmx example.cmx
	$(OCAMLOPT) -c $<

run-socket-2.mli:run-socket-2.ml
	$(OCAMLC) -i $< > $@

run-th.cmo:run-th.ml th.cmo example.cmo
	$(OCAMLC) -c $<

run-th.cmx:run-th.ml th.cmx example.cmx
	$(OCAMLOPT) -c $<

run-th.cmi:run-th.mli
	$(OCAMLC) -c $<

run-th.mli:run-th.ml
	$(OCAMLC) -i $< > $@

semaphore.cmo:semaphore.ml semaphore.ml
	$(OCAMLC) -c $<

semaphore.cmx:semaphore.ml semaphore.ml
	$(OCAMLOPT) -c $<

semaphore.cmi:semaphore.mli
	$(OCAMLC) -c $<

semaphore.mli:semaphore.ml
	$(OCAMLC) -i $< > $@

seq.cmo:seq.ml
	$(OCAMLC) -c $<

seq.cmx:seq.ml seq.ml
	$(OCAMLOPT) -c $<

seq.cmi:seq.mli
	$(OCAMLC) -c $<

seq.mli:seq.ml
	$(OCAMLC) -i $< > $@

serveur.cmo:serveur.ml
	$(OCAMLC) -c $<

serveur.cmx:serveur.ml serveur.ml
	$(OCAMLOPT) -c $<

serveur.cmi:serveur.mli
	$(OCAMLC) -c $<

serveur.mli:serveur.ml
	$(OCAMLC) -i $< > $@

spectrogram.cmo:serveur.ml
	$(OCAMLC) -c $<

spectrogram.cmx:spectrogram.ml serveur.ml
	$(OCAMLOPT) -c $<

spectrogram.cmi:spectrogram.mli
	$(OCAMLC) -c $<

spectrogram.mli:spectrogram.ml
	$(OCAMLC) -i $< > $@

th.cmo:th.ml
	$(OCAMLC) -c $<

th.cmx:th.ml
	echo th.cmx
	$(OCAMLOPT) -c $<

th.cmi:th.mli
	$(OCAMLC) -c $<

th.mli:th.ml
	$(OCAMLC) -i $< > $@

uniqid.cmo:uniqid.ml
	$(OCAMLC) -c $<

uniqid.cmx:uniqid.ml
	$(OCAMLOPT) -c $<

uniqid.cmi:uniqid.mli
	$(OCAMLC) -c $<

uniqid.mli:uniqid.ml
	$(OCAMLC) -i $< > $@

