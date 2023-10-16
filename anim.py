#!/usr/bin/env python3

import os, sys

#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print (str (format) % args, end="")


#
#  fatal - display a message and exit.
#

def fatal (format, *args):
    print (str (format) % args, end="")
    sys.exit (1)


#
#  is_left - returns True if line starts with string, start.
#

def is_left (line, start):
    if len (line) < len (start):
        #
        #  no, the line is too short
        #
        return False
    if len (line) == len (start):
        #
        #  extact length, return direct comparison
        #
        return line == start
    #
    #  line must be larger, safe to split
    #
    return line[:len (start)] == start


def is_space (line):
    return (len (line) >= 1) and (line[0] == " ")


def get_right (line):
    words = line.split (":")
    if len (words) >= 1:
        word = words[1].lstrip ().strip ()
        return word
    return None

#
#  a simple library which is intended to work with Pygame3
#  to provide animated sprites or images.
#

class form:
    def __init__ (self):
        self._anims = {}
        self._levels = []
    #
    #  add_anim - appends anim into the list
    #
    def add_anim (self, anim):
        level = anim.get_level ()
        # printf ("adding anim to level %d\n", level)
        if not (level in self._levels):
            self._levels += [level]
            self._levels.sort ()
        if level in self._anims:
            self._anims[level] += [anim]
        else:
            self._anims[level] = [anim]
    #
    #  update - for every anim in the form update
    #           if the anim has ended call the _finished_func
    #           callback.  It returns True (if a change has occurred
    #           and a display_flip is required to show the new image).
    #
    def update (self, need_flip = True):
        for level in self._levels:
            # printf ("scanning level %d\n", level)
            i = 0
            while i < len (self._anims[level]):
                a = self._anims[level][i]
                # printf ("  image %d\n", i)
                if a.update (need_flip):
                    self._anims[level].remove (a)
                    # print (a)
                    # print (a._finished_func)
                    if a._finished_func != None:
                        a._finished_func (a._entity, a, a._animation_object)
                else:
                    if a.new_frame ():
                        need_flip = True
                    # printf ("onto next image\n")
                    i += 1
        return need_flip  # possibly a new frame has been drawn
    #
    #  finished - return True if all the animations in this
    #             form have completed.
    #
    def finished (self):
        for level in self._levels:
            if self._anims[level] != []:
                # found an active animation.
                return False
        return True

class anim:
    def __init__ (self, animation_object, anim_list, entity, redraw_func = None, finished_func = None):
        assert (type (anim_list) == type([]))
        self._finished_func = finished_func
        self._redraw_func = redraw_func
        self._entity = entity
        self._animation_object = animation_object
        anim_list = [[None, 0]] + anim_list
        self._anim_list = anim_list  # list of [[image, duration], ...]
        self._level = animation_object.get_level ()
        for l in anim_list:
            if len (l) < 2:
                printf ("anim_list must contain sub lists of [image, duration]\n")
        self._tick = 0
        self._sprite_no = 0
    def get_level (self):
        return self._level
    #
    #  update - increments the anim tick and returns True if
    #
    def update (self, force_write):
        # printf ("sprite_no = %d, _anim_list = %s, tick = %d\n", self._sprite_no, self._anim_list, self._tick)
        if self._sprite_no < len (self._anim_list):
            #
            #  have we reached the last image in the anim sequence?
            #
            if self._sprite_no == len (self._anim_list) - 1:
                #
                #  have we reached the end duration of the last frame?
                #
                if self._tick == self._anim_list[-1][1]:
                    #  end of complete anim
                    return True
            #
            #  have we reached the end duration of the current frame?
            #
            if self._tick == self._anim_list[self._sprite_no][1]:
                #
                #  move onto the next image, reset tick duration count
                #
                self._tick = 0
                self._sprite_no += 1
                self._redraw_func (self._entity, self, self._animation_object)
            else:
                #
                #  increment tick count, towards end of the sprite duration.
                #
                self._tick += 1
                if force_write:
                    self._redraw_func (self._entity, self, self._animation_object)
        return False
    #
    #  new_frame - returns True if the current sprite image needs to be
    #              written to the display.
    #
    def new_frame (self):
        return self._tick == 0

    #
    #  _refresh - call the _refresh method on the particular sprite/image
    #
    def refresh (self):
        if self._sprite_no < len (self._anim_list):
            self._redraw (self.entity, self)
    def get_attributes (self):
        if self._sprite_no < len (self._anim_list):
            return self._anim_list[self._sprite_no]
        return None


def no_refresh (args):
    pass


def get_next (line, default_value):
    #   printf ("line = %s\n", line)
    line = line.lstrip ()
    if line == "":
        return "", default_value
    else:
        words = line.split ()
        if words[0] == line:
            return "", line
        return line[len (words[0]):], words[0]


def get_next_int (line, default_value):
    line, value = get_next (line, default_value)
    return line, int (value)


def read_anim (line, lineno):
    line, imagefile = get_next (line, None)
    line, duration = get_next_int (line, 1)
    line, image_x = get_next_int (line, 0)
    line, image_y = get_next_int (line, 0)
    line, object_x = get_next_int (line, 0)
    line, object_y = get_next_int (line, 0)
    line, handle = get_next (line, None)
    return imagefile, duration, image_x, image_y, object_x, object_y, handle


class animation:
    def __init__ (self, name, level = 0, filename = "characters.anim", path = "."):
        self._name = name
        self._path = path
        self._filename = filename
        self._pose = {}
        self._load (name)
        self._level = level
    def get_level (self):
        return self._level
    def _load (self, name):
        if not os.path.exists (self._path):
            fatal ("path does not exist: %s\n", self._path)
        self._fullname = os.path.join (self._path, self._filename)
        if not os.path.exists (self._fullname):
            fatal ("file does not exist: %s\n", self._fullname)
        #
        #  remove comments
        #
        lines = open (self._fullname, "r").readlines ()
        decomment = []
        for line in lines:
            l = line.rstrip ()    # remove any spaces on the right
            l = l.split ("#")[0]  # throw away anything to the right of #
            decomment += [l]
        #
        #  read contents and create anims
        #
        curname, curdir, curpose = None, None, None
        for lineno, line in enumerate (decomment):
            if is_left (line, "Name:"):
                curname = get_right (line)
            # printf ("curname = %s, self._name = %s\n", curname, self._name)
            if curname == self._name:
                # printf ("curdir = %s\n", curdir)
                if is_left (line, "Directory:"):
                    curdir = get_right (line)
                elif is_left (line, "Pose:"):
                    curpose = get_right (line)
                elif is_space (line):
                    # set the default values, as these are optional in the file
                    curfile, curdur, image_dx, image_dy, char_dx, char_dy, handle = read_anim (line, lineno)
                    if curname is None:
                        fatal ("curname is None\n")
                    if not (curname in self._pose):
                        self._pose[curname] = {}
                    if curpose is None:
                        fatal ("curpose is None\n")
                    elif curpose in self._pose[curname]:
                        self._pose[curname][curpose] += [[os.path.join (curdir, curfile), curdur, image_dx, image_dy, char_dx, char_dy, handle]]
                    else:
                        self._pose[curname][curpose] = [[os.path.join (curdir, curfile), curdur, image_dx, image_dy, char_dx, char_dy, handle]]

    def get_pose (self, pose):
        # print (self._pose)
        if self._name in self._pose:
            if pose in self._pose[self._name]:
                return self._pose[self._name][pose]
            fatal ("%s: no pose %s for character %s seen in animation sequence\n", self._filename, pose, self._name)
        else:
            fatal ("%s: character %s not seen in animation sequence\n", self._filename, self._name)
