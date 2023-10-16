#!/usr/bin/env python3

import copy
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
init_state, signup_state, roam_state, map_state = range (4)

NORTH, EAST, SOUTH, WEST = range (4)

### copy.deepcopy(x)


def is_protocol_cmd (line, command):
    if len (line) >= len (command):
        if line == command:
            return True
        return line[:len (command)] == command
    return False


#
#  add_vec - returns a new list containing the sum of the
#            elements in both indices.
#            It returns a+b of the two vectors.
#

def add_vec (pos, vec):
    result = []
    for p, v in zip (pos, vec):
        result += [p+v]
    return result


#
#  equal_vec - return true if a == b.
#              We need to test each element of the vector (list).
#

def equal_vec (a, b):
    for i, j in zip (a, b):
        if i != j:
            return False
    return True


def min_int (a, b):
    if a == None:
        return b
    if b == None:
        return a
    if a < b:
        return a
    return b


class frag:
    def __init__ (self, room_no, offset, walls, doors, treasures, prev_frag):
        self.map_content = None
        self.room_no = room_no
        self.offset = copy.deepcopy (offset)
        self.prev_frag = frag # copy.deepcopy (prev_frag)
        self.walls = walls
        self.doors = doors
        self.treasures = treasures
    def is_resolved (self):
        # are all the room walls known?
        return False
    def get_extents (self):
        return self.get_extents_lower ([0, 0], [None, None])
    def get_extents_lower (offset, max_coords):
        for wall in self.walls:
            maxx = maximum (wall[2] + offset[0], max_coords[0])
            maxy = maximum (wall[3] + offset[1], max_coords[1])
        if self.prev_frag != None:
            return get_extents_lower (add_vec (offset, self.offset), [maxx, maxy])
        return [maxx, maxy]


def is_horiz (line):
    if line[0] == line[2]:
        return False
    return True

def is_vert (line):
    return not is_horiz (line)


inprogress_nav, success_nav, timeout_nav, failure_nav = range (4)

"""
class broadphase_line_navigation:
    #
    #  navigation attempted when the room has not yet been mapped.
    #
    def __init__ (self, dest, the_bot, timeout):
        self._dest_line = dest_line
        self._finished = False
        self._the_bot = the_bot
        self._timeout = get_time () + timeout
        self._reason = inprogress_nav
        self._last_command = None
    def finished (self):
        if self._finished:
            return True
        if point_on_line (self._the_bot.get_pos (), self._dest_line):
            self._reason = success_nav
            self._finished = True
        if get_time () >= self._timeout:
            self._reason = timeout_nav
            self._finished = True
        return False
    def think (self):
        if self.finished ():
            return True
        pos = self._the_bot.get_pos ()
        if is_horiz (self._dest_line):
            if
            if self._dest_line[1] > pos[1]:
                self._the_bot.turn (NORTH)

                    self._the_bot.turn (SOUTH)
            else:
                if self._dest_line[0] > pos[0]:
                    self._the_bot.turn (EAST)
                elif self._dest_line[1] < pos[1]:
                    self._the_bot.turn (WEST)
        else:
            # vertical line
            if self._dest_line[0] > pos[0]:
                self._the_bot.turn (EAST)
            elif self._dest_line[0] < pos[0]:
                self._the_bot.turn (WEST)
            else:
                if self._dest_line[1] > pos[1]:
                    self._the_bot.turn (NORTH)
                elif self._dest_line[1] < pos[1]:
                    self._the_bot.turn (SOUTH)
        self._the_bot.add_cmd ("1", 0.2, action_think)
"""


class broadphase_point_navigation:
    #
    #  navigation attempted when the room has not yet been mapped.
    #
    def __init__ (self, dest, the_bot, timeout):
        self._dest = dest
        self._finished = False
        self._the_bot = the_bot
        self._timeout = get_time () + timeout
        self._reason = inprogress_nav
        self._last_command = None
        self._pos_at_move = None
        self._time_at_move = None
    def finished (self):
        if self._finished:
            return True
        if equal_vec (self._the_bot.get_pos (), self._dest):
            self._reason = success_nav
            self._finished = True
        if get_time () >= self._timeout:
            self._reason = timeout_nav
            self._finished = True
        if self._last_command != None:
            if get_time () > self._time_at_move:
                if equal_vec (self._the_bot.get_pos (), self._pos_at_move):
                    # we have not moved (we must have bumped into something)
                    self._reason = failure_nav
                    self._finished = True
        return False
    def think (self):
        if self.finished ():
            return True
        pos = self._the_bot.get_pos ()
        if is_horiz (self._dest_line):
            if self._dest[1] > pos[1]:
                self._the_bot.turn (NORTH)
            elif self._dest[1] < pos[1]:
                self._the_bot.turn (SOUTH)
            else:
                if self._dest[0] > pos[0]:
                    self._the_bot.turn (EAST)
                elif self._dest[1] < pos[1]:
                    self._the_bot.turn (WEST)
        else:
            # vertical line
            if self._dest[0] > pos[0]:
                self._the_bot.turn (EAST)
            elif self._dest[0] < pos[0]:
                self._the_bot.turn (WEST)
            else:
                if self._dest[1] > pos[1]:
                    self._the_bot.turn (NORTH)
                elif self._dest[1] < pos[1]:
                    self._the_bot.turn (SOUTH)
        self._the_bot.add_cmd ("1", 0.2, action_think)
        self._last_command = "1"
        self._pos_at_move = pos
        self._time_at_move = get_time () + 0.2


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
        self._frag = None
        self._last_command = None
        self._direction = 0
        self._nav = None
    def debug (self):
        _printf ("_classname: %s\n", self._classname)
        _printf ("_name: %s\n", self._name)
        _printf ("_sckt: %s\n", self._sckt)
        _printf ("_file_d: %s\n", self._file_d)
        _printf ("_walls: %s\n", self._walls)
        _printf ("_commands: %s\n", self._commands)
        _printf ("_state: %s\n", self._state)
        _printf ("_current_line: %s\n", self._current_line)
        _printf ("_room_no: %s\n", self._room_no)
        _printf ("_goal_stack: %s\n", self._goal_stack)
        _printf ("_my_pos: %s\n", self._my_pos)
        _printf ("_treasures: %s\n", self._treasures)
        _printf ("_frag: %s\n", self._frag)
        _printf ("_last_command: %s\n", self._last_command)
        _printf ("_direction: %s\n", self._direction)
    def add_cmd (self, command, delay=0.0, function=None):
        _printf ("add_cmd (%s, %f, %s)\n", command, delay, function)
        self._commands += [[command, delay, function]]
        self.debug ()
    def send_cmd (self, command):
        if command == 'r':
            self._direction += 1
            self._direction %= 4
        elif command == 'l':
            self._direction += 3
            self._direction %= 4
        elif command == 'v':
            self._direction += 2
            self._direction %= 4
        self._last_command = command
        _printf ("sending command: %s, %s\n", command, self._sckt)
        self._sckt.send (to_bytes (str_encode (command)))
    def add_char (self, ch):
        if ch == "\n":
            _printf ("current line=%s\n", self._current_line)
            self.add_knowledge (self._current_line)
            self._current_line = ""
        else:
            self._current_line += ch
    def push_goal (self, goal_fun):
        self._goal_stack += [goal_fun]
    def set_my_pos (self, pos, direction = None):
        self._my_pos = pos
        if direction != None:
            self._direction = direction
    def get_pos (self):
        return self._my_pos
    def add_knowledge (self, line):
        _printf ("line = %s\n", line)
        words = line.split ()
        if is_protocol_cmd (line, "dR"):
            self._room_no = int (line.split ()[1])
            _printf ("room no = %d\n", self._room_no)
        elif is_protocol_cmd (line, "nman"):
            _printf ("seen North facing man\n")
            self.set_my_pos ([int (words[1]), int (words[2])], 0)
        elif is_protocol_cmd (line, "sman"):
            self.set_my_pos ([int (words[1]), int (words[2])], 2)
        elif is_protocol_cmd (line, "wman"):
            self.set_my_pos ([int (words[1]), int (words[2])], 3)
        elif is_protocol_cmd (line, "eman"):
            self.set_my_pos ([int (words[1]), int (words[2])], 1)
        elif is_protocol_cmd (line, "hwall"):
            self._add_hwall (int (words[1]), int (words[2]), int (words[3]), int (words[4]))
        elif is_protocol_cmd (line, "vwall"):
            self._add_vwall (int (words[1]), int (words[2]), int (words[3]), int (words[4]))
        elif is_protocol_cmd (line, "clear"):
            self._frag = frag (self._room_no, self._my_pos, self._walls, [], [], self._frag)
            self._room_no = None
            self._my_pos = None
            self._frag = None
            self._walls = []
        elif is_protocol_cmd (line, "treasure"):
            self._add_treasure (int (words[1]), int (words[2]))
        elif is_protocol_cmd (line, "combat"):
            if self._state == signup_state:
                _printf ("bot entering combat\n")
                self._state = map_state
    def _add_vwall (self, x0, y0, x1, y1):
        if self.is_known_pos ():
            _printf ("my pos %s and wall %s, %s to %s, %s\n", self._my_pos, x0, y0, x1, y1)
            self._walls += [[x0, y0, x1, y1]]
    def _add_hwall (self, x0, y0, x1, y1):
        if self.is_known_pos ():
            _printf ("my pos %s and wall %s, %s to %s, %s\n", self._my_pos, x0, y0, x1, y1)
            self._walls += [[x0, y0, x1, y1]]
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
            self._state = signup_state
            self.enter_bot ()
        if self._state == map_state:
            if self.is_known_room_no ():
                if self.is_known_room_map ():
                    self.set_dest (get_random_pos (self.get_door (self.get_room_no (), self.get_door_max (self.get_room_no ()))))
                    self._state = nav_state
                    self.push_goal (goal_move_to_dest)
                else:
                    if self._goal_stack == []:
                        self.push_goal (goal_move_to_nearest_wall)
            elif self._commands == []:
                _printf ("unknown room\n")
                self.add_cmd ("^L", 2.0, think)
    def think (self):
        if self._nav != None:
            if self._nav.finished ():
                self._nav = None
            else:
                self._nav.think ()
        else:
            if self._goal_stack == []:
                self.eval_state ()
            if self._goal_stack != []:
                self._goal_stack[0] (self)
    def goal_move_to_nearest_wall (self):
        if self._commands != []:
            return
        if self._walls == []:
            self.move ()
        else:
            wall = self.get_nearest_wall ()
            self._nav = broadphase_point_navigation (wall, self, 10.0)
    def turn (self, direction):
        if direction != self._direction:
            if direction + 2 == self._direction:
                self.add_cmd ("v")
            elif (direction + 1) % 4 == self._direction:
                self.add_cmd ("r")
            elif (direction + 3) % 4 == self._direction:
                self.add_cmd ("l")
    def move (self, amount = "1"):
        if not self.is_known_pos ():
            _printf ("unknown position\n")
            self.add_cmd ("^L", 0.2)
        self.add_cmd (amount, 0.2)
    def get_nearest_wall (self):
        distance = None
        for wall in self._walls:
            if is_horiz (wall):
                if distance != min_int (distance, abs (wall[1] - self._my_pos[1])):
                    distance = min_int (distance, abs (wall[1] - self._my_pos[1]))
                    best = wall
        return best
    def enter_bot (self):
        _printf ("sckt: %s\n", self._sckt)
        self._sckt.send (to_bytes (" "))  # any key to continue
        _printf ("%s setting class\n", self._classname)
        self._sckt.send (to_bytes (self._classname + "\n"))
        _printf ("%s setting name %s\n", self._classname, self._name)
        self._sckt.send (to_bytes (self._name + "\n"))


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
                b.send_cmd (head[0])
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
    bot_ob.goal_move_to_nearest_wall ()


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


def main ():
    bots = {}
    """
    bot_number = {"orc":6,
                  "dwarf": 2}
    """
    bot_number = {"human":1}
    bot_list = []

    for classname in bot_number.keys ():
        for i in range (bot_number[classname]):
            b = bot (classname, "%s" % i)
            bot_list += [b]
    run_bots (bot_list)


main ()
