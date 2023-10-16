#!/usr/bin/env python3

#
#  config and global variables
#

GameTitle       = "Fonthill Tower"
programName     = "fonthilltower"

versionNumber   = 2.0
xmax            = 40    # character viewable area
ymax            = 30    # character viewable area
MaxX            = 800
MaxY            = 600
UnitX           = MaxX / xmax  # should = 20 to match png image sizes
UnitY           = MaxY / ymax  # should = 20 to match png image sizes
Resolution      = (MaxX, MaxY)
FontSize        = 24
StatusWidth     = 180
LineHeight      = 20
White           = (255, 255, 255)
Black           = (0, 0, 0)
historyList     = []    # replay list
lineNo          = 0
columnNo        = [0]
textLine        = ""
fullscreen      = False
serverName      = "localhost"
portNumber      = 7000
debugging       = 0
soundDict       = {}
ConnectAttempts = 10
