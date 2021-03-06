#!/usr/bin/env python
# coding: utf-8

import os, pyglet, wave, struct
import numpy as np
from PIL import Image, ImageDraw
from display_info import *
import variables

to_dir = 'stereograms'
os.makedirs(to_dir, exist_ok=True)

# Input stereogram size in cm unit
size = 5

# Input line size in cm unit
line_length = 0.7  # 30pix is 42 min of arc on 57cm distance

# Input luminance of background
lb = 85  # 215, 84%

# Input fixation point position in cm unit
ecc = 1

sz = round(resolution * (size / d_height))
ll = round(resolution * line_length / d_height)
f = round(sz * 0.023 / 2)  # 3.6 min of arc in 5 deg presentation area, actually 0.6 mm

# Input the disparity at pixel units.
disparity = f*4

eccentricity = round(1 / np.sqrt(2.0) * ecc / d_height * resolution)


# fixation point
def fixation():
    draw.rectangle((int(sz / 2) + eccentricity - f, int(sz / 2) + eccentricity + f * 3,
                    int(sz / 2) + eccentricity + f, int(sz / 2) + eccentricity - f * 3),
                   fill=(0, 0, 255), outline=None)
    draw.rectangle((int(sz / 2) + eccentricity - f * 3, int(sz / 2) + eccentricity + f,
                    int(sz / 2) + eccentricity + f * 3, int(sz / 2) + eccentricity - f),
                   fill=(0, 0, 255), outline=None)


# ls
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

draw.rectangle((int(sz / 2) - int(ll / 2), int(sz / 2) + int(f / 4),
                int(sz / 2) + int(ll / 2), int(sz / 2) - int(f / 4)),
               fill=(int(lb*1.5), 0, 0), outline=None)

fixation()

basename = os.path.basename('test.png')
img.save(os.path.join(to_dir, basename), quality=100)


def disparate(d, d2, name):
    global img, draw
    img = Image.new("RGB", (sz, sz), (lb, lb, lb))
    draw = ImageDraw.Draw(img)

    draw.rectangle((int(sz / 2) - int(ll / 2) + int(f*(d/2)), int(sz / 2) + int(f / 2),
                    int(sz / 2) + int(ll / 2) + int(f*(d/2)), int(sz / 2) - int(f / 2)),
                   fill=(int(lb*1.5), 0, 0), outline=None)

    draw.rectangle((int(sz / 2) - int(ll / 2) + int(f*(d2/2)), int(sz / 2) + int(f / 2) + f*4,
                    int(sz / 2) + int(ll / 2) + int(f*(d2/2)), int(sz / 2) - int(f / 2) + f*4),
                   fill=(0, 0, 0), outline=None)
    draw.rectangle((int(sz / 2) - int(ll / 2) + int(f*(d2/2)), int(sz / 2) + int(f / 2) - f*4,
                    int(sz / 2) + int(ll / 2) + int(f*(d2/2)), int(sz / 2) - int(f / 2) - f*4),
                   fill=(0, 0, 0), outline=None)

    fixation()

    basename = os.path.basename(str(int(np.sqrt(d**2))) + str(d2) + 'ls' + str(name) + '.png')
    img.save(os.path.join(to_dir, basename), quality=100)


for i in variables.variation:
    disparate(0, i, 'L')
    disparate(0, -i, 'R')
    disparate(4, i, 'L')
    disparate(-4, -i, 'R')


# stereogram without stimuli
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

fixation()

to_dir = 'materials'
os.makedirs(to_dir, exist_ok=True)
basename = os.path.basename('pedestal.png')
img.save(os.path.join(to_dir, basename), quality=100)


def disparity_matcher(d, name):
    global img, draw
    img = Image.new("RGB", (sz, sz), (lb, lb, lb))
    draw = ImageDraw.Draw(img)

    draw.rectangle((int(sz / 2) - int(ll / 2) + int(f*d/2), int(sz / 2) + int(f / 2),
                    int(sz / 2) + int(ll / 2) + int(f*d/2), int(sz / 2) - int(f / 2)),
                   fill=(int(lb*1.5), 0, 0), outline=None)

    fixation()

    basename = os.path.basename(str(d) + 'ls' + name + '.png')
    img.save(os.path.join(to_dir, basename), quality=100)


for i in variables.variation2:
    disparity_matcher(i, 'L')
    disparity_matcher(-i, 'R')

# sound files
# special thank: @kinaonao  https://qiita.com/kinaonao/items/c3f2ef224878fbd232f5

# sin波
# --------------------------------------------------------------------------------------------------------------------
def create_wave(A, f0, fs, t, name):  # A:振幅,f0:基本周波数,fs:サンプリング周波数,再生時間[s],n:名前
    # nポイント
    # --------------------------------------------------------------------------------------------------------------------
    point = np.arange(0, fs * t)
    sin_wave = A * np.sin(2 * np.pi * f0 * point / fs)

    sin_wave = [int(x * 32767.0) for x in sin_wave]  # 16bit符号付き整数に変換

    # バイナリ化
    binwave = struct.pack("h" * len(sin_wave), *sin_wave)

    # サイン波をwavファイルとして書き出し
    w = wave.Wave_write(os.path.join(to_dir, str(name) + ".wav"))
    p = (1, 2, fs, len(binwave), 'NONE',
         'not compressed')  # (チャンネル数(1:モノラル,2:ステレオ)、サンプルサイズ(バイト)、サンプリング周波数、フレーム数、圧縮形式(今のところNONEのみ)、圧縮形式を人に判読可能な形にしたもの？通常、 'NONE' に対して 'not compressed' が返されます。)
    w.setparams(p)
    w.writeframes(binwave)
    w.close()


create_wave(1, 460, 44100, 0.2, '460Hz')
create_wave(1, 840, 44100, 0.1, '840Hz')
