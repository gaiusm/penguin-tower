#!/usr/bin/env python3

import anim, argparse, os, pygame, _thread, sys, config

from cliberty import _printf
from pygame.locals import *
from socket import *

sound_dict = {}
history_list = []
anim_queue = []
TOGGLE_ANIM = "~!"   # special character sequence to toggle anim on/off
ANIM_EVENT = USEREVENT+1


def handle_arguments ():
    # handle_arguments create and return the args object.
    parser = argparse.ArgumentParser ()
    parser.add_argument ('-d', '--debug', help='turn on internal debug.',
                         default=False, action='store_true')
    parser.add_argument ('-f', '--full',
                         help='run in full screen rather than a window.',
                         default=False, action='store_true')
    parser.add_argument ('-q', '--quiet',
                         help='disable sound.',
                         default=False, action='store_true')
    parser.add_argument ('-s', '--server',
                         help='the server name running ptower.',
                         default='localhost')
    parser.add_argument ('-n', '--name',
                         help='your player name.',
                         default=None)
    parser.add_argument ('-I', '--include',
                         help='search directory for the game assets.',
                         default=None)
    parser.add_argument ('-p', '--port',
                         help='the server port number.',
                         default=7000)
    parser.add_argument ('-k', '--skin',
                         help='the skin number 1. Male Adv, 2. Female Adv, 3. Male, 4. Female, 5. Zombie, 6. Robot',
                         default=1)
    args = parser.parse_args ()
    return args


def load_sound(name):
    class NoneSound:
        def play(self):
            pass # pass means {} in C, C++ or Java
    if not pygame.mixer or not pygame.mixer.get_init ():
        return NoneSound ()
    return NoneSound () ### --fixme--
    fullname = get_file_name ('sounds', name)
    try:
        sound = pygame.mixer.Sound (fullname)
    except pygame.error as message:
        _printf ('cannot load sound file: %s\n', fullname)
        return NoneSound ()
    return sound


def add_sound (name):
    global sound_dict
    sound_dict[name] = load_sound (name + ".wav")


def init_sounds ():
    for sound in ["arrowswish", "brokenglass",
                  "fuse", "evillaugh", "ohno",
                  "ohnoexplode", "laughexplode",
                  "handgrenade", "magicarrow",
                  "snore", "start", "arrowmiss",
                  "brokenglass", "applause", "exit"]:
        add_sound (sound)


def init_game_client (character, name):
    # init_sounds ()
    init_event_loop (character, name)
    ask_replay_history ()
    finish_game ()


def get_file_name (subdir, filename):
    global args

    if args.include is None:
        return os.path.join (subdir, filename)
    else:
        return os.path.join (args.include, os.path.join (subdir, filename))


def load_png (name):
    """ Load image and return image object"""
    fullname = get_file_name ('data', name)
    try:
        image = pygame.image.load (fullname)
        if image.get_alpha () is None:
            image = image.convert ()
        else:
            image = image.convert_alpha ()
    except pygame.error as message:
        _printf ('Cannot load image: %s\n', fullname)
        raise SystemExit (message)
    return image


def load_static_images ():
    global static_images

    static_images = {}
    for i in ['wallh', 'wallv', 'doorh', 'doorv',
              'hingeh', 'hingev',
              'treasure']:
        static_images[i] = load_png (i + '.png')


def init_graphics ():
    global screen, font, background, fullscreen

    pygame.init ()
    screen = pygame.display.set_mode (config.Resolution, config.fullscreen)
    pygame.display.set_caption (config.GameTitle)
    font = pygame.font.Font (None, config.FontSize)
    pygame.mouse.set_visible (False)
    load_static_images ()
    background = pygame.Surface (screen.get_size ())


def add_queue (entry):
    global anim_queue

    if anim_queue == []:
        pygame.time.set_timer (ANIM_EVENT, 100)
    anim_queue += [entry]


def movement (words, animation):
    print (words, animation)
    _printf ("unitX = %d\n", config.UnitX)
    add_queue ([anim.anim (player_char, player_char.get_pose (animation),
                           char (config.UnitX * int (words[1]), config.UnitY * int (words[2]),
                                 [animation]), redraw, finished), words[-1]])


def func_nman (words):
    print (words)


def func_eman (words):
    movement (words, "think")


def func_sman (words):
    print (words)


def func_wman (words):
    print (words)


def func_animerun (words):
    movement (words, "run")


function_calls = { "nman": func_nman,
                   "eman": func_eman,
                   "sman": func_sman,
                   "wman": func_wman,
                   "animerun": func_animerun }


def process_line (line, in_combat):
    _printf ("process_line: %s\n", line)
    words = line.split ()
    if len (words) > 0:
        if words[0] == "combat":
            return True
        elif words[0] in function_calls:
            function_calls[words[0]] (words)
    return in_combat


def stripcrlf(s):
    return s[:-1]


def create_socket (count):
    port = args.port
    while True:
        sckt = socket (AF_INET, SOCK_STREAM)
        # _printf ("attempting to connect to %s:%d\n", args.server, port)
        try:
            sckt.connect ((args.server, args.port))
            return sckt
        except error as why:
            _printf ("cannot find server on %s:%d\n", args.server, port)
            count -= 1
            if count > 0:
                port += 1
            else:
                _printf ("cannot connect to %s:%d the server is probably not running or it is not accessible\n",
                         args.server, port)
                pygame.event.post (pygame.event.Event (USEREVENT, serverLine = 'abort'))
                sys.exit (0)


def connect_server (id, count):
    global sckt, args

    # s = open('testscript', 'r')
    # line = s.readline()
    f = sckt.makefile ("rb")
    line = stripcrlf (f.readline ().decode ('utf-8'))
    while line:
        pygame.event.post (pygame.event.Event (USEREVENT, server_line = line))
        pygame.time.set_timer (USEREVENT, 0)
        line = stripcrlf (f.readline ().decode ('utf-8'))
    pygame.event.post (pygame.event.Event (USEREVENT, server_line = "quit"))
    pygame.time.set_timer (USEREVENT, 0)


def handle_direction (direction, sckt, newdir):
    if direction == newdir:
        sckt.send ('1'.encode ('utf-8'))
        return direction, sckt
    else:
        direction = (direction + 4 - newdir) % 4
        if direction == 2:
            sckt.send ('v'.encode ('utf-8'))
        elif direction == 1:
            sckt.send ('l'.encode ('utf-8'))
        else:
            sckt.send ('r'.encode ('utf-8'))
    return newdir, sckt


def update_direction (direction, key):
    if key == 'r':
        return (direction + 1) % 4
    if key == 'l':
        return (direction + 3) % 4
    if key == 'v':
        return (direction + 2) % 4
    return direction


def select_character (sckt, character, name):
    push_back = "\n" + character + "\n"
    if name != None:
        push_back += name + "\n"
    for ch in push_back:
        sckt.send (ch.encode ('utf-8'))
    return sckt


def advance_anim ():
    global form, screen, anim_queue

    if form.finished () and (anim_queue != []):
        form.add_anim (anim_queue[0][0])
        delay = anim_queue[0][1]
        if len (anim_queue) == 1:
            anim_queue = []
        else:
            anim_queue = anim_queue[1:]
    if not form.finished ():
        screen.fill (config.Black)
        if form.update ():
            pygame.display.flip ()
        pygame.time.set_timer (ANIM_EVENT, 100)


def init_event_loop (character, name):
    global sckt, args

    _printf ("init_event_loop = %s\n", character)
    sckt = create_socket (config.ConnectAttempts)
    in_combat = False
    _thread.start_new (connect_server, (1, config.ConnectAttempts))
    direction = 0
    direction_dict = {
        pygame.K_LEFT: 3,
        pygame.K_RIGHT: 1,
        pygame.K_UP: 0,
        pygame.K_DOWN: 2,
    }
    sckt = select_character (sckt, character, name)
    while True:
        for event in pygame.event.get ():
            if (event.type == KEYDOWN):
                if in_combat:
                    if event.key == pygame.K_F11:
                        pygame.display.toggle_fullscreen ()
                    elif event.key == pygame.K_F12:
                        sckt.send (TOGGLE_ANIM.encode ('utf-8'))
                    if event.key in direction_dict:
                        direction, sckt = handle_direction (direction, sckt, direction_dict[event.key])
                    elif event.key < 256:
                        direction = update_direction (direction, chr (event.key))
                        sckt.send (chr (event.key).encode ('utf-8'))
                else:
                    _printf ("sending %c\n", chr (event.key))
                    sckt.send (chr (event.key).encode ('utf-8'))
                if event.key == K_ESCAPE:
                    screen = pygame.display.set_mode ((640, 480))
                    sys.exit (0)
            elif event.type == USEREVENT:
                line = event.server_line
                if args.debug:
                    _printf ("about to process: %s\n", line)
                old_in_combat = in_combat
                in_combat = process_line (line, in_combat)
                if (old_in_combat != in_combat) and in_combat:
                    # turn anim on
                    sckt.send (TOGGLE_ANIM.encode ('utf-8'))
                if line != 'sync':
                    history_list.append ([line, pygame.time.get_ticks ()])
                if line == 'quit':
                    return
                if line == 'abort':
                    sys.exit (0)
            elif event.type == ANIM_EVENT:
                advance_anim ()


skin_name = { 1: "Male adventurer",
              2: "Female adventurer",
              3: "Male person",
              4: "Female person",
              5: "Zombie",
              6: "Robot" }

player_char = None
form = None

class moving:
    def __init__ (self, x_origin, y_origin):
        self.x_origin = x_origin
        self.y_origin = y_origin
        printf ("inside moving constructor:  %g and %g\n", x_origin, y_origin)
    def refresh (self):
        printf ("image: %s for %d ticks\n", self.name, self.unit)


def all_done (args):
    printf ("all_done called: %s\n", args)


class char:
    def __init__ (self, x_offset, y_offset, test_list):
        self.x_offset = x_offset
        self.y_offset = y_offset
        self.test_list = test_list
        self.current = 0
    def get_anim_name (self):
        return self.test_list [self.current]
    def move_next (self):
        self.current += 1
        self.current %= len (self.test_list)


def x_scale (value):
    return int (value * config.MaxX)

def y_scale (value):
    return int (value * config.MaxY)

def redraw (char_object, anim_object, animation_object):
    global screen
    ob = anim_object.get_attributes ()
    image = pygame.image.load (ob[0]).convert ()
    rect = image.get_rect ()
    rect.topleft = [char_object.x_offset, char_object.y_offset]
    char_object.x_offset += ob[2]
    char_object.y_offset += ob[3]
    screen.blit (image, rect)


def finished (char_object, anim_object, animation_object):
    global form
    char_object.move_next ()
    _printf ("moving onto: %s\n", char_object.get_anim_name ())
    if not form.finished ():
        form.add_anim (anim.anim (animation_object, animation_object.get_pose (char_object.get_anim_name ()), char_object, redraw, finished))


def init_animations (name):
    global player_char, form
    player_char = anim.animation (name)
    form = anim.form ()


def main ():
    global args
    args = handle_arguments ()
    init_graphics ()
    init_animations (skin_name[args.skin])
    init_game_client ("human", args.name)

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main ()
