#!/usr/bin/env python3

from animlib import printf
from xml.etree import ElementTree as et
import os

yoffset = 10
ytrim = 10
debugging = False

orig_size = [1730, 1282]   # default size of svg
desired = [50, 80]         # desired size
desired = [80, 100]         # desired size
desired = [100, 120]         # desired size


def safeSystem (format, *args):
    s = str (format) % args
    r = os.system (s)
    if r != 0:
        printf ("failed to execute command: %s\n", s)
        os.sys.exit (1)


def gety (p):
    i = int (p.attrib['y'])
    return i + yoffset


def getheight (p):
    i = int (p.attrib['height'])
    return i - ytrim


def extractPoses (name, svgfile, xmlfile):
    tree = et.parse (xmlfile)
    root = tree.getroot ()

    printf ("extracting images for: %s\n", name)
    safeSystem ("mkdir -p scratch/%s", name)
    safeSystem ("convert -resize 1730x1282 %s scratch/%s/master.pnm", svgfile, name)
    for p in root.findall (".//SubTexture"):
        printf ("[%s]", p.attrib['name'])
        if debugging:
            printf ("%s | %s | %s | %s | %s\n" % (p.attrib['name'],
                                                  p.attrib['x'], p.attrib['y'],
                                                  p.attrib['width'], p.attrib['height']))
        safeSystem ("pnmcut %s %d %s %s scratch/%s/master.pnm | pnmscale -xsize %d -ysize %d | pnmtopng -transparent white > scratch/%s/%s.png",
                    p.attrib['x'], gety (p), p.attrib['width'], getheight (p),
                    name, desired[0], desired[1], name,
                    p.attrib['name'])
    printf ("\n")

def extractAll ():
    safeSystem ("rm -rf scratch")
    extractPoses ("Zombie", "assets/Zombie/Vector/character_zombie.svg", "assets/Zombie/Tilesheet/character_zombie_sheetHD.xml")
    extractPoses ("Female_person", "assets/Female_person/Vector/character_femalePerson.svg", "assets/Female_person/Tilesheet/character_femalePerson_sheetHD.xml")
    extractPoses ("Female_adventurer", "assets/Female_adventurer/Vector/character_femaleAdventurer.svg", "assets/Female_adventurer/Tilesheet/character_femaleAdventurer_sheetHD.xml")
    extractPoses ("Male_person", "assets/Male_person/Vector/character_malePerson.svg", "assets/Male_person/Tilesheet/character_malePerson_sheetHD.xml")
    extractPoses ("Male_adventurer", "assets/Male_adventurer/Vector/character_maleAdventurer.svg", "assets/Male_adventurer/Tilesheet/character_maleAdventurer_sheetHD.xml")
    extractPoses ("Robot", "assets/Robot/Vector/character_robot.svg", "assets/Robot/Tilesheet/character_robot_sheetHD.xml")


extractAll ()
