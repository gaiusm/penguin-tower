#!/usr/bin/env python3


commands = {"quit"  : do_quit,
            "forget": do_forget,
            "set"   : do_set,
            "get"   : do_get,
            "is"    : do_is}

do_is_command = {"abs_pos_known", do_is_abs_pos_known,
                 "room_mapped", do_is_room_mapped,

def do_is (args):
    if len (args) == 0:
        return "is: malformed request"
    return do_is_command[args[0]] (args)


def process_command (command):
