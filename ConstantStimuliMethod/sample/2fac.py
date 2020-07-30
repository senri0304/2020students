# -*- coding: utf-8 -*-
import os, pyglet, time, datetime, random, copy, math
from pyglet.gl import *
from pyglet.image import AbstractImage
from collections import deque
import pandas as pd
import numpy as np
import display_info

# Prefernce
# ------------------------------------------------------------------------
rept = 1
exclude_mousePointer = False
duration = 0.5
latency = 0.2
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
iso = 8
draw_objects = []  # 描画対象リスト
end_routine = False  # Routine status to be exitable or not
response = []  # Count transients
trial_times = []
exitance = True
n = 0

# Load resources
p_sound = pyglet.resource.media('materials/840Hz.wav', streaming=False)
beep_sound = pyglet.resource.media('materials/460Hz.wav', streaming=False)
pedestal: AbstractImage = pyglet.image.load('materials/pedestal.png')
fixr = pyglet.sprite.Sprite(pedestal, x=cntx+iso*deg1-pedestal.width/2.0, y=cnty-pedestal.height/2.0)
fixl = pyglet.sprite.Sprite(pedestal, x=cntx-iso*deg1-pedestal.width/2.0, y=cnty-pedestal.height/2.0)

file_names = copy.copy(display_info.variation)*rept*2
pres_lr = list(np.repeat([1, -1], int(len(file_names)/2)))
r = random.randint(0, math.factorial(len(file_names)))
random.seed(r)
sequence = random.sample(file_names, len(file_names))
random.seed(r)
sequence2 = random.sample(pres_lr, len(file_names))
print(sequence)
print(sequence2)

# ----------- Core program following ----------------------------

# A getting key response function
class key_resp(object):
    def on_key_press(self, symbol, modifiers):
        global tc, exitance, trial_start
        if exitance is False and symbol == key.LEFT: # target in visible
            response.append(1)
            pyglet.clock.schedule_once(get_results, 0.5)
        if exitance is False and symbol == key.RIGHT: # target in invisible
            response.append(0)
            pyglet.clock.schedule_once(get_results, 0.5)
        if exitance and symbol == key.UP:
            p_sound.play()
            pyglet.clock.schedule_once(success, latency)
            pyglet.clock.schedule_once(delete, duration + latency)
            replace()
            trial_start = time.time()
        if symbol == key.ESCAPE:
            win.close()
            pyglet.app.exit()


resp_handler = key_resp()


def fixer():
    draw_objects.append(fixl)
    draw_objects.append(fixr)


def replace():
#    del draw_objects[:]
    fixer()
    draw_objects.append(preceeder)


def success(dt):
    draw_objects.append(successor)


# A end routine function
def exit_routine():
    global exitance
    exitance = True
    beep_sound.play()
    prepare_routine()
    pyglet.app.exit()


@win.event
def on_draw():
    # Refresh window
    win.clear()
    # 描画対象のオブジェクトを描画する
    for draw_object in draw_objects:
        draw_object.draw()


# Remove stimulus
def delete(dt):
    global n, trial_end, exitance
    del draw_objects[:]
    fixer()
    p_sound.play()
    n += 1
    trial_end = time.time()
    exitance = False


def get_results(dt):
    global ku, kud, kd, n, response, trial_end, trial_start, sequence, file_names
    trial_time = trial_end - trial_start
    trial_times.append(trial_time)
    print('--------------------------------------------------')
    print('trial: ' + str(n) + '/' + str(len(file_names)))
    print('response: ' + str(response[-1]))
    print('condition: ' + str(sequence[n-1]))
    print('--------------------------------------------------')
    # Check the experiment continue or break
    if n != len(file_names):
        exit_routine()
    else:
        pyglet.app.exit()


def set_polygon(lr):
    global successor, preceeder, sequence, n
    # Set up polygon for stimulus
    successor = pyglet.resource.image('stereograms/' + str(sequence[n]) + 'ls' + str(sequence2[n]) + '.png')
    successor = pyglet.sprite.Sprite(successor)
    successor.x = cntx + deg1 * iso * lr - successor.width / 2.0
    successor.y = cnty - successor.height / 2.0
    preceeder = pyglet.resource.image('stereograms/' + str(sequence[n]) + 'ls' + str(sequence2[n]) + '.png')
    preceeder = pyglet.sprite.Sprite(preceeder)
    preceeder.x = cntx - deg1 * iso * lr - preceeder.width / 2.0
    preceeder.y = cnty - preceeder.height / 2.0


def prepare_routine():
    global n, file_names
    if n < len(file_names):
        fixer()
        set_polygon(sequence2[n])
    else:
        pass


# Store the start time
start = time.time()
win.push_handlers(resp_handler)

fixer()
set_polygon(sequence2[0])


for i in sequence:
    tc = 0  # Count transients
    ku = deque([])  # Store unix time when key up
    kd = deque([])  # Store unix time when key down
    kud = []  # Differences between kd and ku

    pyglet.app.run()

# -------------- End loop -------------------------------

win.close()

# Store the end time
end_time = time.time()
daten = datetime.datetime.now()

# Write results onto csv
results = pd.DataFrame({'trial': list(range(1, len(file_names)+1)),  # Store variance_A conditions
                        'cnd': sequence,
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