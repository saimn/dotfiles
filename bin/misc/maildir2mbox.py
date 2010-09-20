# -*- coding: utf-8 -*-

"""
maildir2mbox.py
Nathan R. Yergler, 6 June 2010

This file does not contain sufficient creative expression to invoke
assertion of copyright.  No warranty is expressed or implied; use at
your own risk.

---

Uses Python's included mailbox library to convert mail archives from
maildir [http://en.wikipedia.org/wiki/Maildir] to 
mbox [http://en.wikipedia.org/wiki/Mbox] format.

See http://docs.python.org/library/mailbox.html#mailbox.Mailbox for 
full documentation on this library.

---

To run, save as maildir2mbox.py and run:

$ python maildir2mbox.py [maildir_path] [mbox_filename]

[maildir_path] should be the the path to the actual maildir (containing new, cur, tmp);

[mbox_filename] will be newly created.
"""

import mailbox
import sys
import email

# open the existing maildir and the target mbox file
maildir = mailbox.Maildir(sys.argv [-2], email.message_from_file)
mbox = mailbox.mbox(sys.argv[-1])

# lock the mbox
mbox.lock()

# iterate over messages in the maildir and add to the mbox
for msg in maildir:
    mbox.add(msg)

# close and unlock
mbox.close()
maildir.close()
