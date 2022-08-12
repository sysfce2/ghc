TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS))

clean:
	rm -f *.hi *.hi-fat *.o

fat001: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fwrite-if-simplfied-core -dno-typeable-binds
	test -f Fat.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) --show-iface Fat.hi | grep -A3 "extra decls"

# If -fbyte-code-and-object-code is set then we should generate bytecode as the Linkable.
fat005: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -ddump-bcos Fat.hs -fbyte-code-and-object-code
	test -f Fat.o

# Likewise, if -fbyte-code-and-object-code is off then don't produce bytecode
fat007: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -ddump-bcos Fat.hs


fat006: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -dno-typeable-binds -fno-code -fwrite-if-simplfied-core
	test -f Fat.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) --show-iface Fat.hi | grep -A3 "extra decls"
	test ! -f Fat.o

fat006a: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -dno-typeable-binds -fno-code -fwrite-if-simplfied-core -O2

fat008: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) FatTH.hs -fwrite-if-simplfied-core -fprefer-byte-code
	echo >> "FatTH.hs"
	# Observe that FatQuote.hs is not recompiled and the fat interface is used.
	"$(TEST_HC)" $(TEST_HC_OPTS) FatTH.hs -fwrite-if-simplfied-core -fprefer-byte-code


# Same as fat008 but with ghci, broken due to recompilation checking wibbles

fat009: clean
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) FatTH.hs -fwrite-if-simplfied-core
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) FatTH.hs -fwrite-if-simplfied-core

fat010: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) THC.hs -fhide-source-paths -fwrite-if-simplfied-core -fprefer-byte-code
	echo >> "THB.hs"
	"$(TEST_HC)" $(TEST_HC_OPTS) THC.hs -fhide-source-paths -fwrite-if-simplfied-core -fprefer-byte-code

fat014: clean
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v0 -fno-code < fat014.script

