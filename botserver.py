#!/usr/bin/env python3

import os
import select
import time
import sys
from socket import *
from cliberty import _printf

CONTROL_L = chr (12)


"""
commands = {"quit"  : do_quit,
            "forget": do_forget,
            "set"   : do_set,
            "get"   : do_get,
            "is"    : do_is}

do_is_command = {"abs_pos_known", do_is_abs_pos_known,
                 "room_mapped", do_is_room_mapped}

def do_is (args):
    if len (args) == 0:
        return "is: malformed request"
    return do_is_command[args[0]] (args)


def process_command (command):
    pass
"""

serverName = "ettarde"
portNumber = 7000


def get_time ():
    return time.clock_gettime_ns (time.CLOCK_BOOTTIME)


def connect_bot (classname, name):
    global portNumber
    attempts = 10
    _printf ("bot: %s:%s connecting to %s:%d:  " % (classname, name, serverName, portNumber))
    while True:
        sckt = socket (AF_INET, SOCK_STREAM)
        try:
            sckt.connect ((serverName, portNumber))
            _printf ("\n")
            return sckt
        except error as why:
            attempts -= 1
            if attempts > 0:
                _printf (".")
                sys.stdout.flush ()
                portNumber += 1
            else:
                _printf ("cannot connect to %s:%d the server is probably not running or it is not accessible" % (serverName, portNumber))
                sys.exit (0)


# init_state, roam_state, map_state, fight_state, target_state, retreat_state = range (6)
init_state, roam_state, map_state = range (3)

### copy.deepcopy(x)


def is_protocol_cmd (line, command):
    if len (line) >= len (command):
        if line == command:
            return True
        return line[:len (command)] == command
    return False


class bot:
    def __init__ (self, classname, name):
        self._classname = classname
        self._name = name
        self._sckt = connect_bot (classname, name)
        self._file_d = self._sckt.makefile ("b")
        self._walls = []
        self._commands = []
        self._state = init_state
        self._current_line = ""
        self._room_no = None
        self._goal_stack = []
        self._my_pos = None
        self._treasures = []
    def add_cmd (self, command, delay=0.0, function=None):
        self._commands += [[command, delay, function]]
    def add_char (self, ch):
        if ch == "\n":
            _printf ("current line=%s\n", self._current_line)
            self.add_knowledge (self._current_line)
            self._current_line = ""
        else:
            self._current_line += ch
    def push_goal (self, goal_fun):
        self._goal_stack += [goal_fun]
    def add_knowledge (self, line):
        if is_protocol_cmd (line, "dR"):
            self._room_no = int (line.split ()[1])
        elif (is_protocol_cmd (line, "Nman") or is_protocol_cmd (line, "Sman") or
              is_protocol_cmd (line, "Wman") or is_protocol_cmd (line, "Eman")):
            self._my_pos = [int (line.split ()[1]), int (line.split ()[2])]
        elif is_protocol_cmd (line, "hwall"):
            self._add_hwall (int (line.split ()[1]), int (line.split ()[2]), int (line.split ()[3]), int (line.split ()[4]))
        elif is_protocol_cmd (line, "vwall"):
            self._add_vwall (int (line.split ()[1]), int (line.split ()[2]), int (line.split ()[3]), int (line.split ()[4]))
        elif is_protocol_cmd (line, "clear"):
            self._room_no = None
            self._my_pos = None
        elif is_protocol_cmd (line, "treasure"):
            self._add_treasure (int (line.split ()[1]), int (line.split ()[2]))
    def _add_vwall (self, x0, y0, x1, y1):
        if self.is_known_pos ():
            _printf ("my pos %s and wall %s, %s to %s, %s\n", self._my_pos, x0, y0, x1, y1)
    def _add_hwall (self, x0, y0, x1, y1):
        if self.is_known_pos ():
            _printf ("my pos %s and wall %s, %s to %s, %s\n", self._my_pos, x0, y0, x1, y1)
    def _add_treasure (self, x, y):
        _printf ("seen treasure at: %d, %d\n", x, y)
        self._treasures += [[x, y, get_time ()]]
    def set_dest (self, pos):
        self._dest = pos
    def get_dest (self, pos):
        return self._dest
    def is_known_room_no (self):
        return self._room_no != None
    def is_known_room_map (self):
        return False
    def is_known_pos (self):
        return self._my_pos != None
    def eval_state (self):
        if self._state == init_state:
            self._state = map_state
        if self._state == map_state:
            if self.is_known_room_no ():
                if self.is_known_room_map ():
                    self.set_dest (get_random_pos (self.get_door (self.get_room_no (), self.get_door_max (self.get_room_no ()))))
                    self._state = nav_state
                    self.push_goal (goal_move_to_dest)
                else:
                    self._state = map_state
                    self.push_goal (goal_move_to_nearest_wall)
            else:
                self.add_cmd ("^L", 0.2, think)
    def think (self):
        if self._goal_stack == []:
            self.eval_state ()
        if self._goal_stack != []:
            self._goal_stack[0] (self)


def to_bytes (format, *args):
    s = str (format) % args
    return s.encode ('utf-8')


def find_shortest_delay (bot_list):
    shortest = None
    for b in bot_list:
        if b._commands != []:
            if (shortest == None) or (shortest > b._commands[0][1]):
                shortest = b._commands[0][1]
    return shortest


def create_bot_list (bot_list, fd_list):
    new_list = []
    for b in bot_list:
        if b._file_d in fd_list:
            new_list += [b]
    return new_list


def get_input_fds (bot_list):
    new_list = []
    for b in bot_list:
        new_list += [b._file_d]
    return new_list


def wait_for (bot_list, shortest_delay):
    input_fds = get_input_fds (bot_list)
    before_time = time.monotonic ()
    ready_list, y, z = select.select (input_fds, [], [], shortest_delay)
    after_time = time.monotonic ()
    return create_bot_list (bot_list, input_fds), after_time-before_time


def str_encode (s):
    if s == "^L":
        return CONTROL_L
    return s


def remove_delay (bot_list, delay):
    for b in bot_list:
        if b._commands == []:
            b.think ()
        else:
            head = b._commands[0]
            if len (b._commands) > 1:
                tail = b._commands[1:]
            else:
                tail = []
            if head[1] > delay:
                head = [[head[0], head[1]-delay, head[2]]]
            else:
                b._sckt.send (to_bytes (str_encode (head[0])))
                if head[2] != None:
                    head[2] (b)
                head = []
            b._commands = head + tail


def service_interrupt (bot_list, interrupt_list):
    for b in interrupt_list:
        ready = None
        while ready != []:
            ready, y, z = select.select ([b._file_d], [], [], 0)
            if ready != []:
                ch = b._file_d.read (1)
                ch = ch.decode ('utf-8')
                b.add_char (ch)
                # _printf ("%s:%s: %c\n", b._classname, b._name, ch)
    return bot_list


    """
    if bot_ob._state == init_state:
        bot_ob._state = map_state
    if  bot_ob._commands == []:
        bot_ob.add_cmd ("l", 0.2)
        bot_ob.add_cmd ("3", 0.2, eval_state)
    """


def goal_move_to_nearest_wall (bot_ob):
    _printf ("[%s] goal: move to nearest wall\n", bot_ob._name)


def goal_move_to_dest (bot_ob):
    _printf ("[%s] goal: move to dest: %s\n", bot_ob._name, bot_ob.get_dest ())


def think_bots (bot_list):
    for bot_ob in bot_list:
        bot_ob.think ()

def think (bot_ob):
    bot_ob.think ()

def run_bots (bot_list):
    while bot_list != []:
        think_bots (bot_list)
        shortest_delay = find_shortest_delay (bot_list)
        interrupt_list, delay = wait_for (bot_list, shortest_delay)
        remove_delay (bot_list, delay)
        bot_list = service_interrupt (bot_list, interrupt_list)


def enter_bot (b):
    b._sckt.send (to_bytes (" "))  # any key to continue
    _printf ("%s setting class\n", b._classname)
    b._sckt.send (to_bytes (b._classname + "\n"))
    _printf ("%s setting name %s\n", b._classname, b._name)
    b._sckt.send (to_bytes (b._name + "\n"))


def main ():
    bots = {}
    """
    bot_number = {"orc":6,
                  "dwarf": 2}
    """
    bot_number = {"human":10}
    bot_list = []

    for classname in bot_number.keys ():
        for i in range (bot_number[classname]):
            b = bot (classname, "%s" % i)
            bot_list += [b]
    for b in bot_list:
        enter_bot (b)
    run_bots (bot_list)


main ()
