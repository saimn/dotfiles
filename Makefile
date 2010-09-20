# http://github.com/cofi/dotfiles/blob/master/Makefile

.PHONY: all emacs-compile elisp-compile deploy backup

PWD := `pwd`
LINK_CMD := ln --symbolic --force -T
BCKUP_CMD := cp -v --recursive
BCKUP_DIR := dotfiles_backup
NORMAL_FILES := `ls -I README -I Makefile -I bin -I mozilla -I config`

refresh:
	emacs --batch --no-site-file --eval '(byte-recompile-directory "emacs.d/")'

all: compile deploy

compile:
	emacs --batch --no-site-file --eval '(byte-recompile-directory "emacs.d/" 0 t)'

deploy:
	for file in $(NORMAL_FILES); do $(LINK_CMD) $(PWD)/$$file ~/.$$file; done
	for file in `ls config/`; do $(LINK_CMD) $(PWD)/config/$$file ~/.config/$$file; done
	$(LINK_CMD) $(PWD)/bin ~/bin
	$(LINK_CMD) $(PWD)/mozilla/firefox/userContent.css ~/.mozilla/firefox/*/chrome/userContent.css

backup:
	mkdir -p ../$(BCKUP_DIR)/config
	for file in $(NORMAL_FILES); do $(BCKUP_CMD) ~/.$$file ../$(BCKUP_DIR)/$$file; done
	for file in `ls config/`; do $(BCKUP_CMD) ~/.config/$$file ../$(BCKUP_DIR)/config/$$file; done
	$(BCKUP_CMD) ~/bin ../$(BCKUP_DIR)/
