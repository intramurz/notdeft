PROGRAM := notdeft-xapian

default : build

-include local.mk

build : $(PROGRAM)

$(PROGRAM) : notdeft-xapian.cc
	c++ -o $@ $< -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`

clean :
	-rm $(PROGRAM)

install-deps :
	sudo aptitude install pkg-config libtclap-dev libxapian-dev
