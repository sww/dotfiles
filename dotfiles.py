#!/usr/bin/env python

import hashlib
import logging
import os
import os.path
import shutil
import sys

from settings import settings

DIR_VARS = {'home': os.environ['HOME']}

logging.basicConfig(format='%(message)s', level=logging.INFO)
log = logging.getLogger(__name__)

def sha1_checksum(path):
    with open(path) as f:
        h = hashlib.sha1()
        h.update(f.read())

    return h.hexdigest()

def interactive_copy(src, dst, options):
    """Prompt if file exists."""
    dst = dst.format(**DIR_VARS)

    for f in os.listdir(src):
        src_path = os.path.join(src, f)
        dst_path = os.path.join(dst, f)
        log.debug('Source is: %s', src_path)
        if os.path.exists(dst_path):
            if os.path.isdir(src_path):
                pass
            else:
                if sha1_checksum(src_path) == sha1_checksum(dst_path):
                    log.info('files are the same')
                    continue

            if not options.yes and raw_input('%s exists, overwrite? [y/N] ' % dst_path).strip().lower() not in ['y', 'yes']:
                continue

        if os.path.isdir(src_path):
            log.info('* Copied dir "%s" to "%s"', src_path, dst_path)
            shutil.copytree(src_path, dst)
        else:
            log.info('* Copied file "%s" to "%s"', src_path, dst_path)
            shutil.copy(src_path, dst)

    return True

def geaux(args=None, options=None):
    OS = os.uname()[0]

    dotfiles = settings

    if args:
        dotfiles = {}
        for arg in args:
            if arg not in settings:
                sys.stderr.write('"%s" does not exist in the configs\n' % arg)
                continue

            dotfiles[arg] = settings.get(arg)

    for directory, stuff in dotfiles.iteritems():
        if stuff.get('os') and stuff.get('os') != OS:
            continue

        src = os.path.join(os.environ['PWD'], directory)
        dst = stuff.get('dest')
        interactive_copy(src, dst, options)

if __name__ == '__main__':
    import optparse

    parser = optparse.OptionParser()
    parser.add_option('-y', action='store_true', dest='yes', default=False,
                      help='Do not prompt when overwriting files')

    options, args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)
    geaux(args, options)
