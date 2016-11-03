SFP_DEMOS=balls conway fractals sdcard wolf
FULL_DEMOS=2048 $(SFP_DEMOS)

BUILD=Production
LOADER=ROM

GPRBUILD=gprbuild -XBUILD=$(BUILD) -XLOADER=$(LOADER)

all: sfp full

sfp:
	for f in $(SFP_DEMOS); do \
	  for p in $$f/*.gpr; do \
	    echo $$p; \
            $(GPRBUILD) -XRTS=ravenscar-sfp -P $$p -p -q -j0; \
	  done; \
	done

full:
	for f in $(FULL_DEMOS); do \
	  for p in $$f/*.gpr; do \
	    echo $$p; \
            $(GPRBUILD) -XRTS=ravenscar-full -P $$p -p -q -j0; \
	  done; \
	done

clean:
	for f in $(FULL_DEMOS); do \
	  for p in $$f/*.gpr; do \
	    echo $$p; \
	    gprclean -XRTS=ravenscar-full -P $$p -q -r; \
	  done; \
	done
	for f in $(SFP_DEMOS); do \
	  for p in $$f/*.gpr; do \
	    echo $$p; \
	    gprclean -XRTS=ravenscar-sfp -P $$p -q -r; \
	  done; \
	done
