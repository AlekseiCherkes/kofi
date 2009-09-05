PROGRAM := hw
HSFLAGS :=

all:
	ghc --make $(HSFLAGS) -o $(PROGRAM) Main.hs

clean:
	rm -R -f *.hc *.hi *.o *.ho
	rm -f $(PROGRAM)