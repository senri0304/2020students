#!/usr/bin/env python
# coding: utf-8

import pyglet.canvas
import numpy as np

# Input display information
inch = 23
aspect_width = 16
aspect_height = 9

# Input a variety
# size variation = [1, 2]
# eccentricity = [1, 2]

# Input stereogram size in cm unit
size = 5

# Input line size in cm unit
line_length = 0.7  # 30pix is 42 min of arc on 57cm distance

# Input a number you like to initiate
s = 1

# Input luminance of background
lb = 85  # 215, 84%


# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()

resolution = screens[len(screens) - 1].height

c = (aspect_width ** 2 + aspect_height ** 2) ** 0.5
d_height = 2.54 * (aspect_height / c) * inch

deg1 = round(resolution * (1 / d_height))

resolution = screens[len(screens) - 1].height

c = (aspect_width ** 2 + aspect_height ** 2) ** 0.5
d_height = 2.54 * (aspect_height / c) * inch

sz = round(resolution * (size / d_height))
ll = round(resolution * line_length / d_height)
f = round(sz * 0.023 / 2)  # 3.6 min of arc in 5 deg presentation area, actually 0.6 mm

eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)