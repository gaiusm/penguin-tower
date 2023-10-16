#!/usr/bin/env python3

import anim
import sys, pygame
from pygame.locals import *


width  = 1600
height = 1000
size   = width, height
speed1 = [2, 2]
speed2 = [2, 2]
black  = 0, 0, 0
default_background = black


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print (str (format) % args, end="")


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
    return int (value * width)

def y_scale (value):
    return int (value * height)

def redraw (char_object, anim_object, animation_object):
    global screen
    # printf ("redraw called with %s and %s\n", char_object, anim_object)
    # printf ("%s\n", anim_object.get_attributes ())
    ob = anim_object.get_attributes ()
    image = pygame.image.load (ob[0]).convert ()
    rect = image.get_rect ()
    rect.topleft = [char_object.x_offset, char_object.y_offset]
    char_object.x_offset += ob[2]
    char_object.y_offset += ob[3]
    screen.blit (image, rect)


def finished (char_object, anim_object, animation_object):
    global f
    char_object.move_next ()
    # printf ("moving onto: %s\n", char_object.get_anim_name ())
    f.add_anim (anim.anim (animation_object, animation_object.get_pose (char_object.get_anim_name ()), char_object, redraw, finished))


def main ():
    global screen, robot, f
    pygame.init ()
    screen = pygame.display.set_mode (size)

    male_adv = anim.animation ("Male adventurer", -1)
    female_adv = anim.animation ("Female adventurer")
    male_person = anim.animation ("Male person")
    female_person = anim.animation ("Female person")
    zombie = anim.animation ("Zombie", -1)
    robot = anim.animation ("Robot", 1)
    f = anim.form ()
    f.add_anim (anim.anim (male_adv, male_adv.get_pose ("walk"),
                           char (x_scale (.6), y_scale (.4),
                                 ["cheer", "duck", "walk", "walk", "fall", "walk", "walk", "run", "think", "talk", "climb", "attack", "interact", "walk", "walk"]), redraw, finished))
    f.add_anim (anim.anim (female_adv, female_adv.get_pose ("run"),
                           char (x_scale (.7), y_scale (.7), ["climb", "run", "cheer", "walk", "run", "think", "talk", "climb", "attack"]), redraw, finished))
    f.add_anim (anim.anim (male_person, male_person.get_pose ("walk"),
                           char (x_scale (.7), y_scale (.4), ["think", "walk", "run", "think", "talk", "climb", "attack"]), redraw, finished))
    f.add_anim (anim.anim (female_person, female_person.get_pose ("run"),
                           char (x_scale (.4), y_scale (.7), ["run", "cheer", "walk", "run", "think", "talk", "climb", "attack"]), redraw, finished))
    f.add_anim (anim.anim (robot, robot.get_pose ("walk"),
                           char (x_scale (.3), y_scale (.1), ["attack", "walk", "run", "think", "talk", "climb", "attack"]), redraw, finished))
    f.add_anim (anim.anim (zombie, zombie.get_pose ("run"),
                           char (x_scale (.4), y_scale (.1), ["cheer", "run", "cheer", "walk", "run", "think", "talk", "climb", "attack"]), redraw, finished))

    screen.fill (black)
    f.update (True)
    while not f.finished ():
        # printf ("tick\n")
        pygame.time.delay (50)
        if f.update ():
            pygame.display.flip ()
            screen.fill (default_background)
        for event in pygame.event.get ():
            if event.type == pygame.QUIT:
                sys.exit (0)
            if event.type == KEYDOWN:
                sys.exit (0)



main ()
