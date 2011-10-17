# http://github.com/cofi/dotfiles/blob/master/Makefile

.PHONY: all emacs-compile emacs-clean deploy backup clean

PWD := `pwd`
LINK_CMD := ln --symbolic --force -T
CLEAN_CMD := rm -rfv
BCKUP_CMD := cp -v --recursive
BCKUP_DIR := dotfiles_backup
NORMAL_FILES := `ls -I README -I Makefile -I bin -I config -I conky -I dzen`

all: backup clean emacs-compile deploy

emacs-compile:
	# emacs -Q --batch --eval '(byte-recompile-directory "emacs.d/site-lisp/" 0 t)'
	# emacs -Q -L . -batch -f batch-byte-compile `find . -name "*.el"`
	emacs -Q -batch -f batch-byte-compile `find ~/.emacs.d/site-lisp -name "*.el"`

emacs-refresh:
	emacs --batch --eval '(byte-recompile-directory "emacs.d/")'

emacs-clean:
	find emacs.d/ -name "*.elc" -delete

clean:
	for file in $(NORMAL_FILES); do $(CLEAN_CMD) ~/.$$file; done
	for file in `ls config/`; do $(CLEAN_CMD) ~/.config/$$file; done
	$(CLEAN_CMD) ~/bin
	$(CLEAN_CMD) ~/.mozilla/firefox/*/chrome/userContent.css

deploy:
	for file in $(NORMAL_FILES); do $(LINK_CMD) $(PWD)/$$file ~/.$$file; done
	for file in `ls config/`; do $(LINK_CMD) $(PWD)/config/$$file ~/.config/$$file; done
	$(LINK_CMD) $(PWD)/bin ~/bin
	#$(LINK_CMD) $(PWD)/mozilla/firefox/userContent.css ~/.mozilla/firefox/*/chrome/userContent.css

backup:
	mkdir -p ../$(BCKUP_DIR)/config
	for file in $(NORMAL_FILES); do $(BCKUP_CMD) ~/.$$file ../$(BCKUP_DIR)/$$file; done
	for file in `ls config/`; do $(BCKUP_CMD) ~/.config/$$file ../$(BCKUP_DIR)/config/$$file; done
	$(BCKUP_CMD) ~/bin ../$(BCKUP_DIR)/
