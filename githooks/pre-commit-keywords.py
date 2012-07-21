#!/usr/bin/env python

import re
import subprocess
import sys

BAD_KEYWORDS = ['pdb', r'\bconsole\.log\b', r'\bprint\b']

errors = False
string = 'Found bad keyword in file %s:\t%s'
if sys.stdout.isatty():
    string = 'Found bad keyword in file %s:\t\x1b[31;1m%s\x1b[0m' # Red.

bad_re = re.compile('|'.join([str(word) for word in BAD_KEYWORDS]))

proc = subprocess.Popen('git diff --cached', shell=True, stdout=subprocess.PIPE)
diff = proc.communicate()[0]

filename = None
for line in diff.split('\n'):
    cleaned_line = line.strip()
    if cleaned_line.startswith('+++ b/'):
        # This line contains the filename.
        filename = cleaned_line.split('b/', 1)[-1]
    elif cleaned_line.startswith('+'):
        result = bad_re.findall(cleaned_line)
        if result:
            print string % (filename, ', '.join(result))
            errors = True

if errors:
    sys.exit(1)

sys.exit(0)
