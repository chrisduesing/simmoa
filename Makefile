all:
	(cd src;$(MAKE);cp *.hrl ../ebin)

clean:
	(cd src;$(MAKE) clean;cp *.hrl ../ebin)
