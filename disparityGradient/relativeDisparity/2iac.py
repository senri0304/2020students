# -*- coding: utf-8 -*-
import os, pyglet, time, datetime, random, copy, math
from pyglet.gl import *
from pyglet.image import AbstractImage
from collections import deque
import pandas as pd
import numpy as np
import variables, display_info

# Prefernce
# ------------------------------------------------------------------------
rept = 10
exclude_mousePointer = False
duration = 1
# ------------------------------------------------------------------------

# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()
win = pyglet.window.Window(style=pyglet.window.Window.WINDOW_STYLE_BORDERLESS)
win.set_fullscreen(fullscreen=True, screen=screens[len(screens)-1])  # Present secondary display
win.set_exclusive_mouse(exclude_mousePointer)  # Exclude mouse pointer
key = pyglet.window.key

# Load variable conditions
deg1 = display_info.deg1
cntx = screens[len(screens)-1].width / 2  # Store center of screen about x position
cnty = screens[len(screens)-1].height / 3  # Store center of screen about y position
dat = pd.DataFrame()
iso = 8.0
draw_objects = []  # 描画対象リスト
end_routine = False  # Routine status to be exitable or not
response = []  # Count transients
trial_times = []
exit = True
oneshot = True
n = 0

# Load resources
p_sound = pyglet.resource.media('materials/840Hz.wav', streaming=False)
beep_sound = pyglet.resource.media('materials/460Hz.wav', streaming=False)
pedestal: AbstractImage = pyglet.image.load('materials/pedestal.png')
fixr = pyglet.sprite.Sprite(pedestal, x=cntx+iso*deg1-pedestal.width/2.0, y=cnty-pedestal.height/2.0)
fixl = pyglet.sprite.Sprite(pedestal, x=cntx-iso*deg1-pedestal.width/2.0, y=cnty-pedestal.height/2.0)

depth_match = copy.copy(variables.variation2)  #list(np.repeat(variables.variation2, len(variables.variation)))
depth_match *= len(variables.variation)*len(variables.variation3)
file_names = list(np.repeat(variables.variation, len(variables.variation2)))
file_names *= len(variables.variation3)
tar_depth = list(np.repeat(variables.variation3, len(file_names)/2))

depth_match *= rept
file_names *= rept
tar_depth *= rept

r = random.randint(0, math.factorial(len(file_names)))
random.seed(r)
sequence = random.sample(file_names, len(file_names))
random.seed(r)
sequence2 = random.sample(depth_match, len(file_names))
random.seed(r)
sequence3 = random.sample(tar_depth, len(file_names))

print(sequence)
print(sequence2)
print(sequence3)

# A getting key response function
class key_resp(object):
    def on_key_press(self, symbol, modifiers):
        global exit, trial_start, oneshot
        if exit is False and oneshot and symbol == key.LEFT: # target in visible
            response.append(1)
            delete()
            get_results()
            exit = True
            oneshot = False
        if exit is False and oneshot and symbol == key.RIGHT: # target in invisible
            response.append(0)
            delete()
            get_results()
            exit = True
            oneshot = False
        if exit and oneshot and symbol == key.UP:
            p_sound.play()
            pyglet.clock.schedule_once(fix, duration)
            pyglet.clock.schedule_once(success, duration + 0.5)
            replace()
            trial_start = time.time()
            oneshot = False
        if symbol == key.ESCAPE:
            win.close()
            pyglet.app.exit()


resp_handler = key_resp()


def fixer():
    draw_objects.append(fixl)
    draw_objects.append(fixr)


def replace():
    fixer()
    draw_objects.append(preceeder)
    draw_objects.append(successor)


def fix(dt):
    del draw_objects[:]
    fixer()


def success(dt):
    global exit, oneshot
    draw_objects.append(dmL)
    draw_objects.append(dmR)
    exit = False
    oneshot = True


# A end routine function
#def exit_routine():
#    global exit
#    exit = True
#    beep_sound.play()
#    prepare_routine()
#    pyglet.app.exit()


@win.event
def on_draw():
    # Refresh window
    win.clear()
    # 描画対象のオブジェクトを描画する
    for draw_object in draw_objects:
        draw_object.draw()


# Remove stimulus
def delete():
    global n, trial_end, oneshot
    del draw_objects[:]
    fixer()
#    p_sound.play()
    n += 1
    trial_end = time.time()
    beep_sound.play()
    prepare_routine()
    pyglet.app.exit()


def get_results():
    global trial_end, trial_start
    trial_time = trial_end - trial_start
    trial_times.append(trial_time)
    print('--------------------------------------------------')
    print('trial: ' + str(n) + '/' + str(len(file_names)))
    print('response: ' + str(response[n-1]))
    print('condition: ' + str(sequence3[n-1]) + ',' + str(sequence[n-1]) + ', ' + str(sequence2[n-1]))
    print('--------------------------------------------------')
    # Check the experiment continue or break
#    if n != len(file_names):
#        exit_routine()
#    else:
#        pyglet.app.exit()


def set_polygon(d, v, v2):
    global successor, preceeder, dmL, dmR
    # Set up polygon for stimulus
    successor = pyglet.resource.image('stereograms/' + str(d) + str(v) + 'lsL.png')
    successor = pyglet.sprite.Sprite(successor)
    successor.x = cntx - deg1 * iso - successor.width / 2.0
    successor.y = cnty - successor.height / 2.0
    preceeder = pyglet.resource.image('stereograms/' + str(d) + str(-v) + 'lsR.png')
    preceeder = pyglet.sprite.Sprite(preceeder)
    preceeder.x = cntx + deg1 * iso - preceeder.width / 2.0
    preceeder.y = cnty - preceeder.height / 2.0

    dmL = pyglet.resource.image('materials/' + str(v2) + 'lsL.png')
    dmL = pyglet.sprite.Sprite(dmL)
    dmL.x = cntx + deg1 * iso - dmL.width / 2.0
    dmL.y = cnty - dmL.height / 2.0
    dmR = pyglet.resource.image('materials/' + str(-v2) + 'lsR.png')
    dmR = pyglet.sprite.Sprite(dmR)
    dmR.x = cntx - deg1 * iso - dmR.width / 2.0
    dmR.y = cnty - dmR.height / 2.0


def prepare_routine():
    if n < len(file_names):
        fixer()
        set_polygon(sequence3[n], sequence[n], sequence2[n])
    else:
        pass


# Store the start time
start = time.time()
win.push_handlers(resp_handler)

fixer()
set_polygon(sequence3[0], sequence[0], sequence2[0])


for i in sequence:
    oneshot = True
    pyglet.app.run()

# -------------- End loop -------------------------------

win.close()

# Store the end time
end_time = time.time()
daten = datetime.datetime.now()

# Write results onto csv
results = pd.DataFrame({'trial': list(range(1, len(file_names)+1)),  # Store variance_A conditions
                        'cnd': sequence,
                        'stimulated_eye': sequence2,
                        'response': response, # Store transient_counts
                        'trial_times': trial_times})

os.makedirs('data', exist_ok=True)

name = str(daten)
name = name.replace(":", "'")
results.to_csv(path_or_buf='./data/DATE' + name + '.csv', index=False)  # Output experimental data

# Output following to shell, check this experiment
print(u'開始日時: ' + str(start))
print(u'終了日時: ' + str(end_time))
print(u'経過時間: ' + str(end_time - start))
