#!/usr/bin/env python3

import sys


#
#  _printf - keeps C programmers happy :-)
#

def _printf (format, *args):
    print (str (format) % args, end="")


#
#  _debugf - _printf when debugging is True
#

def _debugf (format, *args):
    global debugging

    if debugging:
        print (str (format) % args, end="")

#
#  _errorf - generate an error
#

def _errorf (format, *args):
    m = str (format) % args
    sys.stdout.write ("error: " + m)
    sys.exit (1)

#
#  _internalf - generate an internal error and raise an exception.
#

def _internalf (format, *args):
    m = str (format) % args
    sys.stdout.write ("internal error: " + m)
    sys.exit (1)
