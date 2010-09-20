byte-compile:
	emacs -Q -L . -batch -f batch-byte-compile `find . -name "*.el"`
	#emacs -batch -no-site-file -l init.el -f batch-byte-compile `find . -name "*.el"`

clean:
	find . -name "*.elc" -delete
