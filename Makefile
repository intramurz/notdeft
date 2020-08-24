default : clean install

# Override this to add custom load paths to the required libraries, or perhaps to load the init file with "emacs --batch -l ~/.emacs".
EMACS_BATCH := emacs --batch

-include local.mk

install :
	$(EMACS_BATCH) -L . -l notdeft-install -f notdeft-install-autoloads -f notdeft-install-bytecode

exe :
	$(MAKE) -C xapian

clean :
	-rm notdeft-autoloads.el *.elc extras/*.elc

PKGVER := $(shell date +%Y%m%d.%H%M)
PKGNAMEVER := notdeft-$(PKGVER)
PKGTMPDIR := /tmp/$(PKGNAMEVER)
PKGMANIFEST := $(PKGTMPDIR)/notdeft-pkg.el

package :
	mkdir -p download
	-rm -r $(PKGTMPDIR)
	mkdir -p $(PKGTMPDIR)
	cp -ai ./ $(PKGTMPDIR)/
	( cd $(PKGTMPDIR) && git clean -dxffq && rm -rf .git && rm .gitignore Makefile )
	echo '(define-package "notdeft" "'$(PKGVER)'"' > $(PKGMANIFEST)
	echo '  "Note manager and search engine")' >> $(PKGMANIFEST)
	( tar --create --file download/$(PKGNAMEVER).tar -C /tmp $(PKGNAMEVER) )
