OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

all:th pr seq socket-1

th:
	$(OCAMLBUILD) run-th.$(TARGET)

seq:
	$(OCAMLBUILD) run-seq.$(TARGET)

pr:
	$(OCAMLBUILD) run-pr.$(TARGET)

socket-1:
	$(OCAMLBUILD) run-socket-1.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
