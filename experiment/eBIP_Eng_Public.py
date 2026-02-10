#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2024.2.4),
    on January 23, 2025, at 11:12
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

# --- Import packages ---
from psychopy import locale_setup
from psychopy import prefs
from psychopy import plugins
plugins.activatePlugins()
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout, hardware
from psychopy.tools import environmenttools
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER, priority)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard

# --- Setup global variables (available in all functions) ---
# create a device manager to handle hardware (keyboards, mice, mirophones, speakers, etc.)
deviceManager = hardware.DeviceManager()
# ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
# store info about the experiment session
psychopyVersion = '2024.2.4'
expName = 'eBIP_Eng_Public'  # from the Builder filename that created this script
# information about this experiment
expInfo = {
    'participant': 'Optional',
    'email': 'Optional@gmail.com',
    'gender (*)': 'Female',
    'age (*)': '25',
    'highest degree obtained (high school = 12) (*)': '12',
    'right or left handedness (*)': 'right',
    'profession (*)': 'profession',
    'date|hid': data.getDateStr(),
    'expName|hid': expName,
    'psychopyVersion|hid': psychopyVersion,
}

# --- Define some variables which will change depending on pilot mode ---
'''
To run in pilot mode, either use the run/pilot toggle in Builder, Coder and Runner, 
or run the experiment with `--pilot` as an argument. To change what pilot 
#mode does, check out the 'Pilot mode' tab in preferences.
'''
# work out from system args whether we are running in pilot mode
PILOTING = core.setPilotModeFromArgs()
# start off with values from experiment settings
_fullScr = True
_winSize = [1280, 800]
# if in pilot mode, apply overrides according to preferences
if PILOTING:
    # force windowed mode
    if prefs.piloting['forceWindowed']:
        _fullScr = False
        # set window size
        _winSize = prefs.piloting['forcedWindowSize']

def showExpInfoDlg(expInfo):
    """
    Show participant info dialog.
    Parameters
    ==========
    expInfo : dict
        Information about this experiment.
    
    Returns
    ==========
    dict
        Information about this experiment.
    """
    # show participant info dialog
    dlg = gui.DlgFromDict(
        dictionary=expInfo, sortKeys=False, title=expName, alwaysOnTop=True
    )
    if dlg.OK == False:
        core.quit()  # user pressed cancel
    # return expInfo
    return expInfo


def setupData(expInfo, dataDir=None):
    """
    Make an ExperimentHandler to handle trials and saving.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    dataDir : Path, str or None
        Folder to save the data to, leave as None to create a folder in the current directory.    
    Returns
    ==========
    psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    # remove dialog-specific syntax from expInfo
    for key, val in expInfo.copy().items():
        newKey, _ = data.utils.parsePipeSyntax(key)
        expInfo[newKey] = expInfo.pop(key)
    
    # data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
    if dataDir is None:
        dataDir = _thisDir
    filename = u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])
    # make sure filename is relative to dataDir
    if os.path.isabs(filename):
        dataDir = os.path.commonprefix([dataDir, filename])
        filename = os.path.relpath(filename, dataDir)
    
    # an ExperimentHandler isn't essential but helps with data saving
    thisExp = data.ExperimentHandler(
        name=expName, version='',
        extraInfo=expInfo, runtimeInfo=None,
        originPath='S:\\vol2015\\bbf2518\\Downloads\\eBIP_Eng_Public-main\\eBIP_Eng_Public-main\\eBIP_Eng_Public.py',
        savePickle=True, saveWideText=True,
        dataFileName=dataDir + os.sep + filename, sortColumns='time'
    )
    thisExp.setPriority('thisRow.t', priority.CRITICAL)
    thisExp.setPriority('expName', priority.LOW)
    # return experiment handler
    return thisExp


def setupLogging(filename):
    """
    Setup a log file and tell it what level to log at.
    
    Parameters
    ==========
    filename : str or pathlib.Path
        Filename to save log file and data files as, doesn't need an extension.
    
    Returns
    ==========
    psychopy.logging.LogFile
        Text stream to receive inputs from the logging system.
    """
    # set how much information should be printed to the console / app
    if PILOTING:
        logging.console.setLevel(
            prefs.piloting['pilotConsoleLoggingLevel']
        )
    else:
        logging.console.setLevel('warning')
    # save a log file for detail verbose info
    logFile = logging.LogFile(filename+'.log')
    if PILOTING:
        logFile.setLevel(
            prefs.piloting['pilotLoggingLevel']
        )
    else:
        logFile.setLevel(
            logging.getLevel('exp')
        )
    
    return logFile


def setupWindow(expInfo=None, win=None):
    """
    Setup the Window
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    win : psychopy.visual.Window
        Window to setup - leave as None to create a new window.
    
    Returns
    ==========
    psychopy.visual.Window
        Window in which to run this experiment.
    """
    if PILOTING:
        logging.debug('Fullscreen settings ignored as running in pilot mode.')
    
    if win is None:
        # if not given a window to setup, make one
        win = visual.Window(
            size=_winSize, fullscr=_fullScr, screen=0,
            winType='pyglet', allowGUI=False, allowStencil=False,
            monitor='testMonitor', color=[1.0000, 1.0000, 1.0000], colorSpace='rgb',
            backgroundImage='', backgroundFit='none',
            blendMode='avg', useFBO=True,
            units='height',
            checkTiming=False  # we're going to do this ourselves in a moment
        )
    else:
        # if we have a window, just set the attributes which are safe to set
        win.color = [1.0000, 1.0000, 1.0000]
        win.colorSpace = 'rgb'
        win.backgroundImage = ''
        win.backgroundFit = 'none'
        win.units = 'height'
    if expInfo is not None:
        # get/measure frame rate if not already in expInfo
        if win._monitorFrameRate is None:
            win._monitorFrameRate = win.getActualFrameRate(infoMsg='Attempting to measure frame rate of screen, please wait...')
        expInfo['frameRate'] = win._monitorFrameRate
    win.hideMessage()
    # show a visual indicator if we're in piloting mode
    if PILOTING and prefs.piloting['showPilotingIndicator']:
        win.showPilotingIndicator()
    
    return win


def setupDevices(expInfo, thisExp, win):
    """
    Setup whatever devices are available (mouse, keyboard, speaker, eyetracker, etc.) and add them to 
    the device manager (deviceManager)
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window in which to run this experiment.
    Returns
    ==========
    bool
        True if completed successfully.
    """
    # --- Setup input devices ---
    ioConfig = {}
    
    # Setup iohub keyboard
    ioConfig['Keyboard'] = dict(use_keymap='psychopy')
    
    # Setup iohub experiment
    ioConfig['Experiment'] = dict(filename=thisExp.dataFileName)
    
    # Start ioHub server
    ioServer = io.launchHubServer(window=win, **ioConfig)
    
    # store ioServer object in the device manager
    deviceManager.ioServer = ioServer
    
    # create a default keyboard (e.g. to check for escape)
    if deviceManager.getDevice('defaultKeyboard') is None:
        deviceManager.addDevice(
            deviceClass='keyboard', deviceName='defaultKeyboard', backend='iohub'
        )
    if deviceManager.getDevice('volume_response') is None:
        # initialise volume_response
        volume_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='volume_response',
        )
    # create speaker 'sound_1'
    deviceManager.addDevice(
        deviceName='sound_1',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('key_test_show') is None:
        # initialise key_test_show
        key_test_show = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_test_show',
        )
    if deviceManager.getDevice('begin_response') is None:
        # initialise begin_response
        begin_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='begin_response',
        )
    if deviceManager.getDevice('vivid_response') is None:
        # initialise vivid_response
        vivid_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='vivid_response',
        )
    if deviceManager.getDevice('vviq_instruction_response') is None:
        # initialise vviq_instruction_response
        vviq_instruction_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='vviq_instruction_response',
        )
    if deviceManager.getDevice('key_resp_vviq') is None:
        # initialise key_resp_vviq
        key_resp_vviq = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp_vviq',
        )
    if deviceManager.getDevice('vviq_feedback_response') is None:
        # initialise vviq_feedback_response
        vviq_feedback_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='vviq_feedback_response',
        )
    if deviceManager.getDevice('imagery_instruction_proceed_2') is None:
        # initialise imagery_instruction_proceed_2
        imagery_instruction_proceed_2 = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='imagery_instruction_proceed_2',
        )
    # create speaker 'imagery_task_trials_word1'
    deviceManager.addDevice(
        deviceName='imagery_task_trials_word1',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'imagery_task_trials_word2'
    deviceManager.addDevice(
        deviceName='imagery_task_trials_word2',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'imagery_task_trials_instruction'
    deviceManager.addDevice(
        deviceName='imagery_task_trials_instruction',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('imagery_task_trials_response') is None:
        # initialise imagery_task_trials_response
        imagery_task_trials_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='imagery_task_trials_response',
        )
    if deviceManager.getDevice('imagery_task_trials_vividness_response') is None:
        # initialise imagery_task_trials_vividness_response
        imagery_task_trials_vividness_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='imagery_task_trials_vividness_response',
        )
    if deviceManager.getDevice('imagery_task_prepare_response') is None:
        # initialise imagery_task_prepare_response
        imagery_task_prepare_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='imagery_task_prepare_response',
        )
    # create speaker 'imagery_task_word1'
    deviceManager.addDevice(
        deviceName='imagery_task_word1',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'imagery_task_word2'
    deviceManager.addDevice(
        deviceName='imagery_task_word2',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'imagery_task_instruction'
    deviceManager.addDevice(
        deviceName='imagery_task_instruction',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('key_resp_vmi') is None:
        # initialise key_resp_vmi
        key_resp_vmi = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp_vmi',
        )
    if deviceManager.getDevice('vividness') is None:
        # initialise vividness
        vividness = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='vividness',
        )
    if deviceManager.getDevice('imagery_task_feedback_response') is None:
        # initialise imagery_task_feedback_response
        imagery_task_feedback_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='imagery_task_feedback_response',
        )
    if deviceManager.getDevice('perception_instruction_response') is None:
        # initialise perception_instruction_response
        perception_instruction_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='perception_instruction_response',
        )
    # create speaker 'perception_task_word1'
    deviceManager.addDevice(
        deviceName='perception_task_word1',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'perception_task_word2'
    deviceManager.addDevice(
        deviceName='perception_task_word2',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'perception_task_instruction'
    deviceManager.addDevice(
        deviceName='perception_task_instruction',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('perception') is None:
        # initialise perception
        perception = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='perception',
        )
    if deviceManager.getDevice('confidence') is None:
        # initialise confidence
        confidence = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='confidence',
        )
    if deviceManager.getDevice('perception_task_feedback_response') is None:
        # initialise perception_task_feedback_response
        perception_task_feedback_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='perception_task_feedback_response',
        )
    if deviceManager.getDevice('semantics_instruction_response') is None:
        # initialise semantics_instruction_response
        semantics_instruction_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='semantics_instruction_response',
        )
    # create speaker 'semantics_task_trials_word1'
    deviceManager.addDevice(
        deviceName='semantics_task_trials_word1',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'semantics_task_trials_word2'
    deviceManager.addDevice(
        deviceName='semantics_task_trials_word2',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'semantics_task_trials_instruction'
    deviceManager.addDevice(
        deviceName='semantics_task_trials_instruction',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('semantics_task_trials_response') is None:
        # initialise semantics_task_trials_response
        semantics_task_trials_response = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='semantics_task_trials_response',
        )
    # create speaker 'field_s'
    deviceManager.addDevice(
        deviceName='field_s',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'word1_s'
    deviceManager.addDevice(
        deviceName='word1_s',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'word2_s'
    deviceManager.addDevice(
        deviceName='word2_s',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    # create speaker 'instr_s'
    deviceManager.addDevice(
        deviceName='instr_s',
        deviceClass='psychopy.hardware.speaker.SpeakerDevice',
        index=-1
    )
    if deviceManager.getDevice('key_resp_vmi_s') is None:
        # initialise key_resp_vmi_s
        key_resp_vmi_s = deviceManager.addDevice(
            deviceClass='keyboard',
            deviceName='key_resp_vmi_s',
        )
    # return True if completed successfully
    return True

def pauseExperiment(thisExp, win=None, timers=[], playbackComponents=[]):
    """
    Pause this experiment, preventing the flow from advancing to the next routine until resumed.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window for this experiment.
    timers : list, tuple
        List of timers to reset once pausing is finished.
    playbackComponents : list, tuple
        List of any components with a `pause` method which need to be paused.
    """
    # if we are not paused, do nothing
    if thisExp.status != PAUSED:
        return
    
    # start a timer to figure out how long we're paused for
    pauseTimer = core.Clock()
    # pause any playback components
    for comp in playbackComponents:
        comp.pause()
    # make sure we have a keyboard
    defaultKeyboard = deviceManager.getDevice('defaultKeyboard')
    if defaultKeyboard is None:
        defaultKeyboard = deviceManager.addKeyboard(
            deviceClass='keyboard',
            deviceName='defaultKeyboard',
            backend='ioHub',
        )
    # run a while loop while we wait to unpause
    while thisExp.status == PAUSED:
        # sleep 1ms so other threads can execute
        clock.time.sleep(0.001)
    # if stop was requested while paused, quit
    if thisExp.status == FINISHED:
        endExperiment(thisExp, win=win)
    # resume any playback components
    for comp in playbackComponents:
        comp.play()
    # reset any timers
    for timer in timers:
        timer.addTime(-pauseTimer.getTime())


def run(expInfo, thisExp, win, globalClock=None, thisSession=None):
    """
    Run the experiment flow.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    psychopy.visual.Window
        Window in which to run this experiment.
    globalClock : psychopy.core.clock.Clock or None
        Clock to get global time from - supply None to make a new one.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    # mark experiment as started
    thisExp.status = STARTED
    # make sure window is set to foreground to prevent losing focus
    win.winHandle.activate()
    # make sure variables created by exec are available globally
    exec = environmenttools.setExecEnvironment(globals())
    # get device handles from dict of input devices
    ioServer = deviceManager.ioServer
    # get/create a default keyboard (e.g. to check for escape)
    defaultKeyboard = deviceManager.getDevice('defaultKeyboard')
    if defaultKeyboard is None:
        deviceManager.addDevice(
            deviceClass='keyboard', deviceName='defaultKeyboard', backend='ioHub'
        )
    eyetracker = deviceManager.getDevice('eyetracker')
    # make sure we're running in the directory for this experiment
    os.chdir(_thisDir)
    # get filename from ExperimentHandler for convenience
    filename = thisExp.dataFileName
    frameTolerance = 0.001  # how close to onset before 'same' frame
    endExpNow = False  # flag for 'escape' or other condition => quit the exp
    # get frame duration from frame rate in expInfo
    if 'frameRate' in expInfo and expInfo['frameRate'] is not None:
        frameDur = 1.0 / round(expInfo['frameRate'])
    else:
        frameDur = 1.0 / 60.0  # could not measure, so guess
    
    # Start Code - component code to be run after the window creation
    
    # --- Initialize components for Routine "resource_manage" ---
    
    # --- Initialize components for Routine "volume_test" ---
    volume_title = visual.TextStim(win=win, name='volume_title',
        text='Volume Calibration',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    volume_response = keyboard.Keyboard(deviceName='volume_response')
    volume_text = visual.TextStim(win=win, name='volume_text',
        text='Make sure you can hear the music and that the volume is correct.\n\nWe recommend that you wear headphones for this test and turn the sound up to a comfortable level for you.',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    volume_continue = visual.TextStim(win=win, name='volume_continue',
        text="Press the < Space > key to continue. \nIf you don't hear the music, press the <Esc> key.\n",
        font='Arial',
        pos=(0, -0.35), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-3.0);
    sound_1 = sound.Sound(
        'audioStims/mariotest2.wav', 
        secs=-1, 
        stereo=True, 
        hamming=False, 
        speaker='sound_1',    name='sound_1'
    )
    sound_1.setVolume(8)
    
    # --- Initialize components for Routine "key_test" ---
    key_test_title = visual.TextStim(win=win, name='key_test_title',
        text='Keyboard Calibration',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='red', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    key_test_text = visual.TextStim(win=win, name='key_test_text',
        text="During this test, you will have to use the following keys on the TOP LEFT part of your keyboard:\n              '1',  '2',  '3',  '4',  '5'\n\nPress the buttons one by one, make sure your keyboard works well.\n\nThe button you just pressed is:\n\n\n",
        font='Arial',
        pos=(0, 0.1), draggable=False, height=0.035, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    key_test_display = visual.TextStim(win=win, name='key_test_display',
        text=None,
        font='Arial',
        pos=(0, -0.05), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    key_test_show = keyboard.Keyboard(deviceName='key_test_show')
    key_test_continue = visual.TextStim(win=win, name='key_test_continue',
        text='If your keyboard is working, press <Enter>.\n',
        font='Arial',
        pos=(0, -0.5), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    key_test_image = visual.ImageStim(
        win=win,
        name='key_test_image', 
        image='img/clavier.jpg', mask=None, anchor='center',
        ori=0, pos=(0, -0.3), draggable=False, size=(0.65, 0.3),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-6.0)
    
    # --- Initialize components for Routine "begin_instruction" ---
    begin_text = visual.TextStim(win=win, name='begin_text',
        text="Let's start the first part of the test, are you ready?",
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    begin_response = keyboard.Keyboard(deviceName='begin_response')
    begin_continue = visual.TextStim(win=win, name='begin_continue',
        text='Proceed by pressing < Space >.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    
    # --- Initialize components for Routine "vivid_instruction" ---
    vivid_title = visual.TextStim(win=win, name='vivid_title',
        text='Part 1/4: Evaluation of Vividness of mental imagery  ',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    vivid_response = keyboard.Keyboard(deviceName='vivid_response')
    vivid_mona_lisa = visual.TextStim(win=win, name='vivid_mona_lisa',
        text='For example, imagine the painting Mona Lisa.',
        font='Arial',
        pos=(0, 0.33), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    vivid_rating = visual.TextStim(win=win, name='vivid_rating',
        text='We will describe scenarios that you will have to imagine in your mind. Then we will ask you to give a score from 1 to 5 to evaluate its liveliness.\n\nAssess the vividness of your mental image with a score from 1 to 5:\n\n1: No image is visible.\n2: the image is vague and imprecise.\n3: The image is moderately clear and sharp.\n4: the image is relatively clear, almost as sharp and precise as a visual perception.\n5: the image is perfectly clear, as sharp and precise as real visual perception. ',
        font='Arial',
        pos=(0, 0.1), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-3.0);
    vivid_image = visual.ImageStim(
        win=win,
        name='vivid_image', 
        image='img/vivacite.jpg', mask=None, anchor='center',
        ori=0, pos=(0, -0.28), draggable=False, size=(0.95, 0.2),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-4.0)
    vivid_text = visual.TextStim(win=win, name='vivid_text',
        text='Enter a rating (1-5) to continue. Use the TOP LEFT part of your keyboard.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    
    # --- Initialize components for Routine "vviq_instruction" ---
    vviq_instruction_title = visual.TextStim(win=win, name='vviq_instruction_title',
        text='Part 1/4: Vividness Test',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    vviq_instruction_text = visual.TextStim(win=win, name='vviq_instruction_text',
        text="We are going to describe some scenarios to you, which you need to imagine in your mind. \nNext, we'll ask you to rate it from 1 to 5 to assess its vividness\n\nThis task takes about 3 minutes.",
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    vviq_instruction_proceed = visual.TextStim(win=win, name='vviq_instruction_proceed',
        text='Press the < Space > key to proceed.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    vviq_instruction_response = keyboard.Keyboard(deviceName='vviq_instruction_response')
    vviq_instruction_good = visual.TextStim(win=win, name='vviq_instruction_good',
        text='Good!',
        font='Arial',
        pos=(0, 0.15), draggable=False, height=0.05, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-4.0);
    
    # --- Initialize components for Routine "vviq_task" ---
    vviq_task_title = visual.TextStim(win=win, name='vviq_task_title',
        text='Part 1/4: Test of Mental Imagery Vividness',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    key_resp_vviq = keyboard.Keyboard(deviceName='key_resp_vviq')
    vviq_task_score = visual.TextStim(win=win, name='vviq_task_score',
        text='Give a score of 1 to 5 to evaluate the vividness of your visual mental imagery',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    vviq_task_idx = visual.TextStim(win=win, name='vviq_task_idx',
        text='',
        font='Arial',
        pos=(0, 0.25), draggable=False, height=0.05, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-3.0);
    vviq_task_item = visual.TextStim(win=win, name='vviq_task_item',
        text='',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-4.0);
    vviq_task_senario = visual.TextStim(win=win, name='vviq_task_senario',
        text='',
        font='Arial',
        pos=(0, 0.15), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    
    # --- Initialize components for Routine "vviq_feedback" ---
    vviq_feedback_title = visual.TextStim(win=win, name='vviq_feedback_title',
        text='Part 1/4: Vividness test result',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    # Run 'Begin Experiment' code from vviq_feedback_text
    msg='doh'
    colortext = 'black'
    vviq_feedback_proceed = visual.TextStim(win=win, name='vviq_feedback_proceed',
        text='Press < Space > key to proceed to the second part.',
        font='Arial',
        pos=(0, -0.2), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    vviq_feedback_response = keyboard.Keyboard(deviceName='vviq_feedback_response')
    vviq_feedback_result = visual.TextStim(win=win, name='vviq_feedback_result',
        text='',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='white', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-4.0);
    
    # --- Initialize components for Routine "imagery_instruction" ---
    imagery_instruction_title = visual.TextStim(win=win, name='imagery_instruction_title',
        text='Part 2/4: Visual Mental Imagery Test',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    imagery_instruction_text = visual.TextStim(win=win, name='imagery_instruction_text',
        text='In this second part, we will evaluate your Visual Mental Imagery through tests covering five categories: shape, color, face, letter and map.\n\nFor each category, you will start with a trial. Then, you will need to respond to the following 15 tests.\n\nThis Visual Mental Imagery task will take about 10 minutes.',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    imagery_instruction_proceed = visual.TextStim(win=win, name='imagery_instruction_proceed',
        text='Press < Space > to proceed.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    imagery_instruction_proceed_2 = keyboard.Keyboard(deviceName='imagery_instruction_proceed_2')
    
    # --- Initialize components for Routine "imagery_task_trials" ---
    imagery_task_trials_question = visual.TextStim(win=win, name='imagery_task_trials_question',
        text='',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    imagery_task_trials_word1 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_trials_word1',    name='imagery_task_trials_word1'
    )
    imagery_task_trials_word1.setVolume(8)
    imagery_task_trials_word2 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_trials_word2',    name='imagery_task_trials_word2'
    )
    imagery_task_trials_word2.setVolume(8)
    imagery_task_trials_instruction = sound.Sound(
        'A', 
        secs=-1, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_trials_instruction',    name='imagery_task_trials_instruction'
    )
    imagery_task_trials_instruction.setVolume(8)
    imagery_task_trials_response = keyboard.Keyboard(deviceName='imagery_task_trials_response')
    imagery_task_trials_order_instruction = visual.TextStim(win=win, name='imagery_task_trials_order_instruction',
        text='',
        font='Arial',
        pos=(0, 0.13), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    imagery_task_trials_img1 = visual.ImageStim(
        win=win,
        name='imagery_task_trials_img1', 
        image='default.png', mask=None, anchor='center',
        ori=0, pos=(0, -0.25), draggable=False, size=(0.4, 0.25),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-6.0)
    imagery_task_trials_proceed = visual.TextStim(win=win, name='imagery_task_trials_proceed',
        text='',
        font='Arial',
        pos=(0, -0.45), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-7.0);
    
    # --- Initialize components for Routine "imagery_task_trials_vividness" ---
    imagery_task_trials_vividness_question = visual.TextStim(win=win, name='imagery_task_trials_vividness_question',
        text='',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    imagery_task_trials_vividness_text = visual.TextStim(win=win, name='imagery_task_trials_vividness_text',
        text='',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    imagery_task_trials_vividness_response = keyboard.Keyboard(deviceName='imagery_task_trials_vividness_response')
    
    # --- Initialize components for Routine "resting" ---
    imagery_task_trials_rest = visual.TextStim(win=win, name='imagery_task_trials_rest',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.1, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "imagery_task_prepare" ---
    imagery_task_prepare_title = visual.TextStim(win=win, name='imagery_task_prepare_title',
        text='Part 2/4 : Test of Imagination',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    imagery_task_prepare_text = visual.TextStim(win=win, name='imagery_task_prepare_text',
        text="Bravo!\n\nNow let's start the tests.\n\nTake your time to answer each question.",
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    imagery_task_prepare_response = keyboard.Keyboard(deviceName='imagery_task_prepare_response')
    imagery_task_prepare_proceed = visual.TextStim(win=win, name='imagery_task_prepare_proceed',
        text='When you are ready, press the < Space > key to proceed.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-3.0);
    
    # --- Initialize components for Routine "imagery_task" ---
    imagery_task_title = visual.TextStim(win=win, name='imagery_task_title',
        text='Part 2/4: Test of Visual Mental Imagery',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    imagery_task_idx = visual.TextStim(win=win, name='imagery_task_idx',
        text='',
        font='Arial',
        pos=(0, 0.3), draggable=False, height=0.05, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    imagery_task_word1 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_word1',    name='imagery_task_word1'
    )
    imagery_task_word1.setVolume(8)
    imagery_task_word2 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_word2',    name='imagery_task_word2'
    )
    imagery_task_word2.setVolume(8)
    imagery_task_instruction = sound.Sound(
        'A', 
        secs=-1, 
        stereo=True, 
        hamming=False, 
        speaker='imagery_task_instruction',    name='imagery_task_instruction'
    )
    imagery_task_instruction.setVolume(8)
    imagery_task_text = visual.TextStim(win=win, name='imagery_task_text',
        text='',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    key_resp_vmi = keyboard.Keyboard(deviceName='key_resp_vmi')
    
    # --- Initialize components for Routine "imagery_task_vividness" ---
    imagery_task_vividness_title = visual.TextStim(win=win, name='imagery_task_vividness_title',
        text='Give a score of 1 to 4 to rate the vividness of your image.\n',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    vividness = keyboard.Keyboard(deviceName='vividness')
    
    # --- Initialize components for Routine "resting" ---
    imagery_task_trials_rest = visual.TextStim(win=win, name='imagery_task_trials_rest',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.1, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "imagery_task_feedback" ---
    imagery_task_feedback_title = visual.TextStim(win=win, name='imagery_task_feedback_title',
        text='Part 2/4: Test of Visual Mental Imagery',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    # Run 'Begin Experiment' code from imagery_task_feedback_text
    msg='doh'
    colortext = 'black'
    imagery_task_feedback_text_1 = visual.TextStim(win=win, name='imagery_task_feedback_text_1',
        text='',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='white', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    imagery_task_feedback_response = keyboard.Keyboard(deviceName='imagery_task_feedback_response')
    imagery_task_feedback_proceed = visual.TextStim(win=win, name='imagery_task_feedback_proceed',
        text='When you are ready, press the < Space > key to proceed.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-4.0);
    
    # --- Initialize components for Routine "perception_instruction" ---
    perception_instruction_title = visual.TextStim(win=win, name='perception_instruction_title',
        text='Part 3/4 : Test of Perception\n\n',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    perception_instruction_task = visual.TextStim(win=win, name='perception_instruction_task',
        text="In this third part, we are going to show you pictures of objects, food, celebrities, or written words that we have asked you to imagine throughout the part 2 of this test.\n\nDuring this part, you will answer the questions by comparing the visual characteristics perceived based on the pictures that you will be able to SEE.\n\nThe perception test takes approximately 10 minutes, with 60 trials in total.\n\nAttention, for the map test, select the city indicated by a red dot (whether it appears first or second).\n\nAfter each question, rate the level of CONFIDENCE of your answer:\n1: I am not at all confident in my answer.\n2: I'm a little confident about my answer.\n3: I am confident in my answer.\n4: I am absolutely sure of my answer.",
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    perception_instruction_response = keyboard.Keyboard(deviceName='perception_instruction_response')
    perception_instruction_proceed = visual.TextStim(win=win, name='perception_instruction_proceed',
        text='Press < Space > to proceed. ',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-3.0);
    
    # --- Initialize components for Routine "perception_task" ---
    perception_task_title = visual.TextStim(win=win, name='perception_task_title',
        text='Part 3: Test of Perception',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    perception_task_word1 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='perception_task_word1',    name='perception_task_word1'
    )
    perception_task_word1.setVolume(8)
    perception_task_img1 = visual.ImageStim(
        win=win,
        name='perception_task_img1', 
        image='default.png', mask=None, anchor='center',
        ori=0, pos=(0, 0), draggable=False, size=(1.16, 0.64),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-2.0)
    perception_task_word2 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='perception_task_word2',    name='perception_task_word2'
    )
    perception_task_word2.setVolume(8)
    perception_task_img2 = visual.ImageStim(
        win=win,
        name='perception_task_img2', 
        image='default.png', mask=None, anchor='center',
        ori=0, pos=(0, 0), draggable=False, size=(1.16, 0.64),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-4.0)
    perception_task_instruction = sound.Sound(
        'A', 
        secs=-1, 
        stereo=True, 
        hamming=False, 
        speaker='perception_task_instruction',    name='perception_task_instruction'
    )
    perception_task_instruction.setVolume(8)
    perception_task_img3 = visual.ImageStim(
        win=win,
        name='perception_task_img3', 
        image='default.png', mask=None, anchor='center',
        ori=0, pos=(0, 0), draggable=False, size=(1.16, 0.64),
        color=[1,1,1], colorSpace='rgb', opacity=1,
        flipHoriz=False, flipVert=False,
        texRes=128, interpolate=True, depth=-6.0)
    perception = keyboard.Keyboard(deviceName='perception')
    perception_task_text = visual.TextStim(win=win, name='perception_task_text',
        text='',
        font='Arial',
        pos=(0, -0.45), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-8.0);
    
    # --- Initialize components for Routine "perception_task_confidence" ---
    perception_task_confidence_idx = visual.TextStim(win=win, name='perception_task_confidence_idx',
        text='',
        font='Arial',
        pos=(0, 0.3), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    perception_task_confidence_text = visual.TextStim(win=win, name='perception_task_confidence_text',
        text="Give a score from 1 to 4 to assess your CONFIDENCE.\n\n1: I am not at all confident in my answer.\n2: I'm a little confident about my answer.\n3: I am confident in my answer.\n4: I am absolutely sure of my answer.",
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-1.0);
    confidence = keyboard.Keyboard(deviceName='confidence')
    
    # --- Initialize components for Routine "resting" ---
    imagery_task_trials_rest = visual.TextStim(win=win, name='imagery_task_trials_rest',
        text=None,
        font='Open Sans',
        pos=(0, 0), draggable=False, height=0.1, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    
    # --- Initialize components for Routine "perception_task_feedback" ---
    perception_task_feedback_title = visual.TextStim(win=win, name='perception_task_feedback_title',
        text='Part 3/4 : Test of Perception',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    # Run 'Begin Experiment' code from perception_task_feedback_text
    msg='doh'
    colortext = 'black'
    perception_task_feedback_text_1 = visual.TextStim(win=win, name='perception_task_feedback_text_1',
        text='',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='white', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-2.0);
    perception_task_feedback_response = keyboard.Keyboard(deviceName='perception_task_feedback_response')
    perception_task_feedback_proceed = visual.TextStim(win=win, name='perception_task_feedback_proceed',
        text='Press < Space > to proceed. ',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-4.0);
    
    # --- Initialize components for Routine "sem_instruction" ---
    semantics_instruction_title = visual.TextStim(win=win, name='semantics_instruction_title',
        text='Part 4/4: Test of Semantics',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=1.0, 
        languageStyle='LTR',
        depth=0.0);
    semantics_instruction_task = visual.TextStim(win=win, name='semantics_instruction_task',
        text='In this last part, we will evaluate your association between abstract words.\n\nDuring this part, you will answer the questions by comparing the abstract characteristics based on the semantic relationship.\nFor each trial, you will hear two abstract words after hearing the instruction “Semantics”. Then, you will hear the indication semantic word. \nWhich of them is closer to the indication word?\nWe will start with a practice trial. \n\nThis Semantics Task will take about 3 minutes with 12 trials.',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=1.0, 
        languageStyle='LTR',
        depth=-1.0);
    semantics_instruction_response = keyboard.Keyboard(deviceName='semantics_instruction_response')
    semantics_instruction_proceed = visual.TextStim(win=win, name='semantics_instruction_proceed',
        text='Press <Space> to proceed.',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0.0, 
        color='green', colorSpace='rgb', opacity=1.0, 
        languageStyle='LTR',
        depth=-3.0);
    
    # --- Initialize components for Routine "semantics_task_trials" ---
    semantics_task_trials_question = visual.TextStim(win=win, name='semantics_task_trials_question',
        text='',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    semantics_task_trials_word1 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='semantics_task_trials_word1',    name='semantics_task_trials_word1'
    )
    semantics_task_trials_word1.setVolume(8)
    semantics_task_trials_word2 = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=False, 
        speaker='semantics_task_trials_word2',    name='semantics_task_trials_word2'
    )
    semantics_task_trials_word2.setVolume(8)
    semantics_task_trials_instruction = sound.Sound(
        'A', 
        secs=-1, 
        stereo=True, 
        hamming=False, 
        speaker='semantics_task_trials_instruction',    name='semantics_task_trials_instruction'
    )
    semantics_task_trials_instruction.setVolume(8)
    semantics_task_trials_response = keyboard.Keyboard(deviceName='semantics_task_trials_response')
    semantics_task_trials_order_instruction = visual.TextStim(win=win, name='semantics_task_trials_order_instruction',
        text='',
        font='Arial',
        pos=(0, 0.13), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-5.0);
    semantics_task_trials_proceed = visual.TextStim(win=win, name='semantics_task_trials_proceed',
        text='',
        font='Arial',
        pos=(0, -0.4), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='green', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=-6.0);
    
    # --- Initialize components for Routine "sem_trial" ---
    semantics_task_title = visual.TextStim(win=win, name='semantics_task_title',
        text='Part 4/4: Test of Semantics',
        font='Arial',
        pos=(0, 0.4), draggable=False, height=0.04, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=1.0, 
        languageStyle='LTR',
        depth=0.0);
    semanticBlock_idx = visual.TextStim(win=win, name='semanticBlock_idx',
        text='',
        font='Arial',
        pos=(0, 0.3), draggable=False, height=0.04, wrapWidth=None, ori=0.0, 
        color='black', colorSpace='rgb', opacity=1.0, 
        languageStyle='LTR',
        depth=-1.0);
    fixation = visual.ShapeStim(
        win=win, name='fixation', vertices='cross',
        size=(0.03, 0.03),
        ori=0.0, pos=(0, 0), draggable=False, anchor='center',
        lineWidth=1.0,
        colorSpace='rgb', lineColor='white', fillColor='white',
        opacity=None, depth=-2.0, interpolate=True)
    field_s = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=True, 
        speaker='field_s',    name='field_s'
    )
    field_s.setVolume(1.0)
    word1_s = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=True, 
        speaker='word1_s',    name='word1_s'
    )
    word1_s.setVolume(1.0)
    word2_s = sound.Sound(
        'A', 
        secs=2, 
        stereo=True, 
        hamming=True, 
        speaker='word2_s',    name='word2_s'
    )
    word2_s.setVolume(1.0)
    instr_s = sound.Sound(
        'A', 
        secs=-1, 
        stereo=True, 
        hamming=True, 
        speaker='instr_s',    name='instr_s'
    )
    instr_s.setVolume(1.0)
    key_resp_vmi_s = keyboard.Keyboard(deviceName='key_resp_vmi_s')
    
    # --- Initialize components for Routine "end_session" ---
    end_session_text = visual.TextStim(win=win, name='end_session_text',
        text='This is the end of tests. Thank you for your participation!\n\n\nFeel free to comment on the site or contact us:\n\n- Share your experience of life and your result.\n- Interested in an fMRI neuroimaging test for research.\n- ...\n\n\nWait for the program to close or press the <Space> key to exit after 5 seconds.',
        font='Arial',
        pos=(0, 0), draggable=False, height=0.03, wrapWidth=None, ori=0, 
        color='black', colorSpace='rgb', opacity=1, 
        languageStyle='LTR',
        depth=0.0);
    
    # create some handy timers
    
    # global clock to track the time since experiment started
    if globalClock is None:
        # create a clock if not given one
        globalClock = core.Clock()
    if isinstance(globalClock, str):
        # if given a string, make a clock accoridng to it
        if globalClock == 'float':
            # get timestamps as a simple value
            globalClock = core.Clock(format='float')
        elif globalClock == 'iso':
            # get timestamps in ISO format
            globalClock = core.Clock(format='%Y-%m-%d_%H:%M:%S.%f%z')
        else:
            # get timestamps in a custom format
            globalClock = core.Clock(format=globalClock)
    if ioServer is not None:
        ioServer.syncClock(globalClock)
    logging.setDefaultClock(globalClock)
    # routine timer to track time remaining of each (possibly non-slip) routine
    routineTimer = core.Clock()
    win.flip()  # flip window to reset last flip timer
    # store the exact time the global clock started
    expInfo['expStart'] = data.getDateStr(
        format='%Y-%m-%d %Hh%M.%S.%f %z', fractionalSecondDigits=6
    )
    
    # --- Prepare to start Routine "resource_manage" ---
    # create an object to store info about Routine resource_manage
    resource_manage = data.Routine(
        name='resource_manage',
        components=[],
    )
    resource_manage.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # store start times for resource_manage
    resource_manage.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    resource_manage.tStart = globalClock.getTime(format='float')
    resource_manage.status = STARTED
    thisExp.addData('resource_manage.started', resource_manage.tStart)
    resource_manage.maxDuration = None
    # keep track of which components have finished
    resource_manageComponents = resource_manage.components
    for thisComponent in resource_manage.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "resource_manage" ---
    resource_manage.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            resource_manage.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in resource_manage.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "resource_manage" ---
    for thisComponent in resource_manage.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for resource_manage
    resource_manage.tStop = globalClock.getTime(format='float')
    resource_manage.tStopRefresh = tThisFlipGlobal
    thisExp.addData('resource_manage.stopped', resource_manage.tStop)
    thisExp.nextEntry()
    # the Routine "resource_manage" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "volume_test" ---
    # create an object to store info about Routine volume_test
    volume_test = data.Routine(
        name='volume_test',
        components=[volume_title, volume_response, volume_text, volume_continue, sound_1],
    )
    volume_test.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for volume_response
    volume_response.keys = []
    volume_response.rt = []
    _volume_response_allKeys = []
    sound_1.setSound('audioStims/mariotest2.wav', hamming=False)
    sound_1.setVolume(8, log=False)
    sound_1.seek(0)
    # store start times for volume_test
    volume_test.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    volume_test.tStart = globalClock.getTime(format='float')
    volume_test.status = STARTED
    thisExp.addData('volume_test.started', volume_test.tStart)
    volume_test.maxDuration = None
    # keep track of which components have finished
    volume_testComponents = volume_test.components
    for thisComponent in volume_test.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "volume_test" ---
    volume_test.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *volume_title* updates
        
        # if volume_title is starting this frame...
        if volume_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            volume_title.frameNStart = frameN  # exact frame index
            volume_title.tStart = t  # local t and not account for scr refresh
            volume_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(volume_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            volume_title.status = STARTED
            volume_title.setAutoDraw(True)
        
        # if volume_title is active this frame...
        if volume_title.status == STARTED:
            # update params
            pass
        
        # *volume_response* updates
        waitOnFlip = False
        
        # if volume_response is starting this frame...
        if volume_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            volume_response.frameNStart = frameN  # exact frame index
            volume_response.tStart = t  # local t and not account for scr refresh
            volume_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(volume_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            volume_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(volume_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(volume_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if volume_response.status == STARTED and not waitOnFlip:
            theseKeys = volume_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _volume_response_allKeys.extend(theseKeys)
            if len(_volume_response_allKeys):
                volume_response.keys = _volume_response_allKeys[-1].name  # just the last key pressed
                volume_response.rt = _volume_response_allKeys[-1].rt
                volume_response.duration = _volume_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *volume_text* updates
        
        # if volume_text is starting this frame...
        if volume_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            volume_text.frameNStart = frameN  # exact frame index
            volume_text.tStart = t  # local t and not account for scr refresh
            volume_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(volume_text, 'tStartRefresh')  # time at next scr refresh
            # update status
            volume_text.status = STARTED
            volume_text.setAutoDraw(True)
        
        # if volume_text is active this frame...
        if volume_text.status == STARTED:
            # update params
            pass
        
        # *volume_continue* updates
        
        # if volume_continue is starting this frame...
        if volume_continue.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            volume_continue.frameNStart = frameN  # exact frame index
            volume_continue.tStart = t  # local t and not account for scr refresh
            volume_continue.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(volume_continue, 'tStartRefresh')  # time at next scr refresh
            # update status
            volume_continue.status = STARTED
            volume_continue.setAutoDraw(True)
        
        # if volume_continue is active this frame...
        if volume_continue.status == STARTED:
            # update params
            pass
        
        # *sound_1* updates
        
        # if sound_1 is starting this frame...
        if sound_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            sound_1.frameNStart = frameN  # exact frame index
            sound_1.tStart = t  # local t and not account for scr refresh
            sound_1.tStartRefresh = tThisFlipGlobal  # on global time
            # update status
            sound_1.status = STARTED
            sound_1.play(when=win)  # sync with win flip
        
        # if sound_1 is stopping this frame...
        if sound_1.status == STARTED:
            if bool(False) or sound_1.isFinished:
                # keep track of stop time/frame for later
                sound_1.tStop = t  # not accounting for scr refresh
                sound_1.tStopRefresh = tThisFlipGlobal  # on global time
                sound_1.frameNStop = frameN  # exact frame index
                # update status
                sound_1.status = FINISHED
                sound_1.stop()
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[sound_1]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            volume_test.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in volume_test.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "volume_test" ---
    for thisComponent in volume_test.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for volume_test
    volume_test.tStop = globalClock.getTime(format='float')
    volume_test.tStopRefresh = tThisFlipGlobal
    thisExp.addData('volume_test.stopped', volume_test.tStop)
    sound_1.pause()  # ensure sound has stopped at end of Routine
    thisExp.nextEntry()
    # the Routine "volume_test" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "key_test" ---
    # create an object to store info about Routine key_test
    key_test = data.Routine(
        name='key_test',
        components=[key_test_title, key_test_text, key_test_display, key_test_show, key_test_continue, key_test_image],
    )
    key_test.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for key_test_show
    key_test_show.keys = []
    key_test_show.rt = []
    _key_test_show_allKeys = []
    # Run 'Begin Routine' code from key_test_present
    textFill = ''
    # store start times for key_test
    key_test.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    key_test.tStart = globalClock.getTime(format='float')
    key_test.status = STARTED
    thisExp.addData('key_test.started', key_test.tStart)
    key_test.maxDuration = None
    # keep track of which components have finished
    key_testComponents = key_test.components
    for thisComponent in key_test.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "key_test" ---
    key_test.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_test_title* updates
        
        # if key_test_title is starting this frame...
        if key_test_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_title.frameNStart = frameN  # exact frame index
            key_test_title.tStart = t  # local t and not account for scr refresh
            key_test_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_title.status = STARTED
            key_test_title.setAutoDraw(True)
        
        # if key_test_title is active this frame...
        if key_test_title.status == STARTED:
            # update params
            pass
        
        # *key_test_text* updates
        
        # if key_test_text is starting this frame...
        if key_test_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_text.frameNStart = frameN  # exact frame index
            key_test_text.tStart = t  # local t and not account for scr refresh
            key_test_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_text, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_text.status = STARTED
            key_test_text.setAutoDraw(True)
        
        # if key_test_text is active this frame...
        if key_test_text.status == STARTED:
            # update params
            pass
        
        # *key_test_display* updates
        
        # if key_test_display is starting this frame...
        if key_test_display.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_display.frameNStart = frameN  # exact frame index
            key_test_display.tStart = t  # local t and not account for scr refresh
            key_test_display.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_display, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_display.status = STARTED
            key_test_display.setAutoDraw(True)
        
        # if key_test_display is active this frame...
        if key_test_display.status == STARTED:
            # update params
            key_test_display.setText('', log=False)
        
        # *key_test_show* updates
        waitOnFlip = False
        
        # if key_test_show is starting this frame...
        if key_test_show.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_show.frameNStart = frameN  # exact frame index
            key_test_show.tStart = t  # local t and not account for scr refresh
            key_test_show.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_show, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_show.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_test_show.clock.reset)  # t=0 on next screen flip
        if key_test_show.status == STARTED and not waitOnFlip:
            theseKeys = key_test_show.getKeys(keyList=['1','2','3','4','5','left','right','space','t','b','y','g','r','comma','return'], ignoreKeys=None, waitRelease=False)
            _key_test_show_allKeys.extend(theseKeys)
            if len(_key_test_show_allKeys):
                key_test_show.keys = _key_test_show_allKeys[-1].name  # just the last key pressed
                key_test_show.rt = _key_test_show_allKeys[-1].rt
                key_test_show.duration = _key_test_show_allKeys[-1].duration
        # Run 'Each Frame' code from key_test_present
        
        keysGet = key_test_show.keys
        
        if keysGet == 'return':
            continueRoutine = False
        else:
            key_test_display.setText(keysGet)
        
        
        
        
        
        # *key_test_continue* updates
        
        # if key_test_continue is starting this frame...
        if key_test_continue.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_continue.frameNStart = frameN  # exact frame index
            key_test_continue.tStart = t  # local t and not account for scr refresh
            key_test_continue.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_continue, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_continue.status = STARTED
            key_test_continue.setAutoDraw(True)
        
        # if key_test_continue is active this frame...
        if key_test_continue.status == STARTED:
            # update params
            pass
        
        # *key_test_image* updates
        
        # if key_test_image is starting this frame...
        if key_test_image.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_test_image.frameNStart = frameN  # exact frame index
            key_test_image.tStart = t  # local t and not account for scr refresh
            key_test_image.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_test_image, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_test_image.status = STARTED
            key_test_image.setAutoDraw(True)
        
        # if key_test_image is active this frame...
        if key_test_image.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            key_test.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in key_test.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "key_test" ---
    for thisComponent in key_test.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for key_test
    key_test.tStop = globalClock.getTime(format='float')
    key_test.tStopRefresh = tThisFlipGlobal
    thisExp.addData('key_test.stopped', key_test.tStop)
    # check responses
    if key_test_show.keys in ['', [], None]:  # No response was made
        key_test_show.keys = None
    thisExp.addData('key_test_show.keys',key_test_show.keys)
    if key_test_show.keys != None:  # we had a response
        thisExp.addData('key_test_show.rt', key_test_show.rt)
        thisExp.addData('key_test_show.duration', key_test_show.duration)
    thisExp.nextEntry()
    # the Routine "key_test" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "begin_instruction" ---
    # create an object to store info about Routine begin_instruction
    begin_instruction = data.Routine(
        name='begin_instruction',
        components=[begin_text, begin_response, begin_continue],
    )
    begin_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for begin_response
    begin_response.keys = []
    begin_response.rt = []
    _begin_response_allKeys = []
    # store start times for begin_instruction
    begin_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    begin_instruction.tStart = globalClock.getTime(format='float')
    begin_instruction.status = STARTED
    thisExp.addData('begin_instruction.started', begin_instruction.tStart)
    begin_instruction.maxDuration = None
    # keep track of which components have finished
    begin_instructionComponents = begin_instruction.components
    for thisComponent in begin_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "begin_instruction" ---
    begin_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *begin_text* updates
        
        # if begin_text is starting this frame...
        if begin_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            begin_text.frameNStart = frameN  # exact frame index
            begin_text.tStart = t  # local t and not account for scr refresh
            begin_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(begin_text, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'begin_text.started')
            # update status
            begin_text.status = STARTED
            begin_text.setAutoDraw(True)
        
        # if begin_text is active this frame...
        if begin_text.status == STARTED:
            # update params
            pass
        
        # *begin_response* updates
        waitOnFlip = False
        
        # if begin_response is starting this frame...
        if begin_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            begin_response.frameNStart = frameN  # exact frame index
            begin_response.tStart = t  # local t and not account for scr refresh
            begin_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(begin_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            begin_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(begin_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(begin_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if begin_response.status == STARTED and not waitOnFlip:
            theseKeys = begin_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _begin_response_allKeys.extend(theseKeys)
            if len(_begin_response_allKeys):
                begin_response.keys = _begin_response_allKeys[-1].name  # just the last key pressed
                begin_response.rt = _begin_response_allKeys[-1].rt
                begin_response.duration = _begin_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *begin_continue* updates
        
        # if begin_continue is starting this frame...
        if begin_continue.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            begin_continue.frameNStart = frameN  # exact frame index
            begin_continue.tStart = t  # local t and not account for scr refresh
            begin_continue.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(begin_continue, 'tStartRefresh')  # time at next scr refresh
            # update status
            begin_continue.status = STARTED
            begin_continue.setAutoDraw(True)
        
        # if begin_continue is active this frame...
        if begin_continue.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            begin_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in begin_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "begin_instruction" ---
    for thisComponent in begin_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for begin_instruction
    begin_instruction.tStop = globalClock.getTime(format='float')
    begin_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('begin_instruction.stopped', begin_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "begin_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "vivid_instruction" ---
    # create an object to store info about Routine vivid_instruction
    vivid_instruction = data.Routine(
        name='vivid_instruction',
        components=[vivid_title, vivid_response, vivid_mona_lisa, vivid_rating, vivid_image, vivid_text],
    )
    vivid_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for vivid_response
    vivid_response.keys = []
    vivid_response.rt = []
    _vivid_response_allKeys = []
    # store start times for vivid_instruction
    vivid_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    vivid_instruction.tStart = globalClock.getTime(format='float')
    vivid_instruction.status = STARTED
    thisExp.addData('vivid_instruction.started', vivid_instruction.tStart)
    vivid_instruction.maxDuration = None
    # keep track of which components have finished
    vivid_instructionComponents = vivid_instruction.components
    for thisComponent in vivid_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "vivid_instruction" ---
    vivid_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *vivid_title* updates
        
        # if vivid_title is starting this frame...
        if vivid_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vivid_title.frameNStart = frameN  # exact frame index
            vivid_title.tStart = t  # local t and not account for scr refresh
            vivid_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_title.status = STARTED
            vivid_title.setAutoDraw(True)
        
        # if vivid_title is active this frame...
        if vivid_title.status == STARTED:
            # update params
            pass
        
        # *vivid_response* updates
        waitOnFlip = False
        
        # if vivid_response is starting this frame...
        if vivid_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vivid_response.frameNStart = frameN  # exact frame index
            vivid_response.tStart = t  # local t and not account for scr refresh
            vivid_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(vivid_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(vivid_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if vivid_response.status == STARTED and not waitOnFlip:
            theseKeys = vivid_response.getKeys(keyList=['1','2','3','4','5'], ignoreKeys=None, waitRelease=False)
            _vivid_response_allKeys.extend(theseKeys)
            if len(_vivid_response_allKeys):
                vivid_response.keys = _vivid_response_allKeys[-1].name  # just the last key pressed
                vivid_response.rt = _vivid_response_allKeys[-1].rt
                vivid_response.duration = _vivid_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *vivid_mona_lisa* updates
        
        # if vivid_mona_lisa is starting this frame...
        if vivid_mona_lisa.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
            # keep track of start time/frame for later
            vivid_mona_lisa.frameNStart = frameN  # exact frame index
            vivid_mona_lisa.tStart = t  # local t and not account for scr refresh
            vivid_mona_lisa.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_mona_lisa, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_mona_lisa.status = STARTED
            vivid_mona_lisa.setAutoDraw(True)
        
        # if vivid_mona_lisa is active this frame...
        if vivid_mona_lisa.status == STARTED:
            # update params
            pass
        
        # *vivid_rating* updates
        
        # if vivid_rating is starting this frame...
        if vivid_rating.status == NOT_STARTED and tThisFlip >= 3-frameTolerance:
            # keep track of start time/frame for later
            vivid_rating.frameNStart = frameN  # exact frame index
            vivid_rating.tStart = t  # local t and not account for scr refresh
            vivid_rating.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_rating, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_rating.status = STARTED
            vivid_rating.setAutoDraw(True)
        
        # if vivid_rating is active this frame...
        if vivid_rating.status == STARTED:
            # update params
            pass
        
        # *vivid_image* updates
        
        # if vivid_image is starting this frame...
        if vivid_image.status == NOT_STARTED and tThisFlip >= 3-frameTolerance:
            # keep track of start time/frame for later
            vivid_image.frameNStart = frameN  # exact frame index
            vivid_image.tStart = t  # local t and not account for scr refresh
            vivid_image.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_image, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_image.status = STARTED
            vivid_image.setAutoDraw(True)
        
        # if vivid_image is active this frame...
        if vivid_image.status == STARTED:
            # update params
            pass
        
        # *vivid_text* updates
        
        # if vivid_text is starting this frame...
        if vivid_text.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
            # keep track of start time/frame for later
            vivid_text.frameNStart = frameN  # exact frame index
            vivid_text.tStart = t  # local t and not account for scr refresh
            vivid_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vivid_text, 'tStartRefresh')  # time at next scr refresh
            # update status
            vivid_text.status = STARTED
            vivid_text.setAutoDraw(True)
        
        # if vivid_text is active this frame...
        if vivid_text.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            vivid_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in vivid_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "vivid_instruction" ---
    for thisComponent in vivid_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for vivid_instruction
    vivid_instruction.tStop = globalClock.getTime(format='float')
    vivid_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('vivid_instruction.stopped', vivid_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "vivid_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "vviq_instruction" ---
    # create an object to store info about Routine vviq_instruction
    vviq_instruction = data.Routine(
        name='vviq_instruction',
        components=[vviq_instruction_title, vviq_instruction_text, vviq_instruction_proceed, vviq_instruction_response, vviq_instruction_good],
    )
    vviq_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for vviq_instruction_response
    vviq_instruction_response.keys = []
    vviq_instruction_response.rt = []
    _vviq_instruction_response_allKeys = []
    # store start times for vviq_instruction
    vviq_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    vviq_instruction.tStart = globalClock.getTime(format='float')
    vviq_instruction.status = STARTED
    thisExp.addData('vviq_instruction.started', vviq_instruction.tStart)
    vviq_instruction.maxDuration = None
    # keep track of which components have finished
    vviq_instructionComponents = vviq_instruction.components
    for thisComponent in vviq_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "vviq_instruction" ---
    vviq_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *vviq_instruction_title* updates
        
        # if vviq_instruction_title is starting this frame...
        if vviq_instruction_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_instruction_title.frameNStart = frameN  # exact frame index
            vviq_instruction_title.tStart = t  # local t and not account for scr refresh
            vviq_instruction_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_instruction_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_instruction_title.status = STARTED
            vviq_instruction_title.setAutoDraw(True)
        
        # if vviq_instruction_title is active this frame...
        if vviq_instruction_title.status == STARTED:
            # update params
            pass
        
        # *vviq_instruction_text* updates
        
        # if vviq_instruction_text is starting this frame...
        if vviq_instruction_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_instruction_text.frameNStart = frameN  # exact frame index
            vviq_instruction_text.tStart = t  # local t and not account for scr refresh
            vviq_instruction_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_instruction_text, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_instruction_text.status = STARTED
            vviq_instruction_text.setAutoDraw(True)
        
        # if vviq_instruction_text is active this frame...
        if vviq_instruction_text.status == STARTED:
            # update params
            pass
        
        # *vviq_instruction_proceed* updates
        
        # if vviq_instruction_proceed is starting this frame...
        if vviq_instruction_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_instruction_proceed.frameNStart = frameN  # exact frame index
            vviq_instruction_proceed.tStart = t  # local t and not account for scr refresh
            vviq_instruction_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_instruction_proceed, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_instruction_proceed.status = STARTED
            vviq_instruction_proceed.setAutoDraw(True)
        
        # if vviq_instruction_proceed is active this frame...
        if vviq_instruction_proceed.status == STARTED:
            # update params
            pass
        
        # *vviq_instruction_response* updates
        waitOnFlip = False
        
        # if vviq_instruction_response is starting this frame...
        if vviq_instruction_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_instruction_response.frameNStart = frameN  # exact frame index
            vviq_instruction_response.tStart = t  # local t and not account for scr refresh
            vviq_instruction_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_instruction_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_instruction_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(vviq_instruction_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(vviq_instruction_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if vviq_instruction_response.status == STARTED and not waitOnFlip:
            theseKeys = vviq_instruction_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _vviq_instruction_response_allKeys.extend(theseKeys)
            if len(_vviq_instruction_response_allKeys):
                vviq_instruction_response.keys = _vviq_instruction_response_allKeys[-1].name  # just the last key pressed
                vviq_instruction_response.rt = _vviq_instruction_response_allKeys[-1].rt
                vviq_instruction_response.duration = _vviq_instruction_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *vviq_instruction_good* updates
        
        # if vviq_instruction_good is starting this frame...
        if vviq_instruction_good.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_instruction_good.frameNStart = frameN  # exact frame index
            vviq_instruction_good.tStart = t  # local t and not account for scr refresh
            vviq_instruction_good.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_instruction_good, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_instruction_good.status = STARTED
            vviq_instruction_good.setAutoDraw(True)
        
        # if vviq_instruction_good is active this frame...
        if vviq_instruction_good.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            vviq_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in vviq_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "vviq_instruction" ---
    for thisComponent in vviq_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for vviq_instruction
    vviq_instruction.tStop = globalClock.getTime(format='float')
    vviq_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('vviq_instruction.stopped', vviq_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "vviq_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    vviq_trials = data.TrialHandler2(
        name='vviq_trials',
        nReps=1.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions('vviq_task_online.xlsx'), 
        seed=None, 
    )
    thisExp.addLoop(vviq_trials)  # add the loop to the experiment
    thisVviq_trial = vviq_trials.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisVviq_trial.rgb)
    if thisVviq_trial != None:
        for paramName in thisVviq_trial:
            globals()[paramName] = thisVviq_trial[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisVviq_trial in vviq_trials:
        currentLoop = vviq_trials
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisVviq_trial.rgb)
        if thisVviq_trial != None:
            for paramName in thisVviq_trial:
                globals()[paramName] = thisVviq_trial[paramName]
        
        # --- Prepare to start Routine "vviq_task" ---
        # create an object to store info about Routine vviq_task
        vviq_task = data.Routine(
            name='vviq_task',
            components=[vviq_task_title, key_resp_vviq, vviq_task_score, vviq_task_idx, vviq_task_item, vviq_task_senario],
        )
        vviq_task.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for key_resp_vviq
        key_resp_vviq.keys = []
        key_resp_vviq.rt = []
        _key_resp_vviq_allKeys = []
        vviq_task_idx.setText(vviq_idx)
        vviq_task_item.setText(item)
        vviq_task_senario.setText(senario)
        # store start times for vviq_task
        vviq_task.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        vviq_task.tStart = globalClock.getTime(format='float')
        vviq_task.status = STARTED
        thisExp.addData('vviq_task.started', vviq_task.tStart)
        vviq_task.maxDuration = None
        # keep track of which components have finished
        vviq_taskComponents = vviq_task.components
        for thisComponent in vviq_task.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "vviq_task" ---
        # if trial has changed, end Routine now
        if isinstance(vviq_trials, data.TrialHandler2) and thisVviq_trial.thisN != vviq_trials.thisTrial.thisN:
            continueRoutine = False
        vviq_task.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *vviq_task_title* updates
            
            # if vviq_task_title is starting this frame...
            if vviq_task_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                vviq_task_title.frameNStart = frameN  # exact frame index
                vviq_task_title.tStart = t  # local t and not account for scr refresh
                vviq_task_title.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(vviq_task_title, 'tStartRefresh')  # time at next scr refresh
                # update status
                vviq_task_title.status = STARTED
                vviq_task_title.setAutoDraw(True)
            
            # if vviq_task_title is active this frame...
            if vviq_task_title.status == STARTED:
                # update params
                pass
            
            # *key_resp_vviq* updates
            waitOnFlip = False
            
            # if key_resp_vviq is starting this frame...
            if key_resp_vviq.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                key_resp_vviq.frameNStart = frameN  # exact frame index
                key_resp_vviq.tStart = t  # local t and not account for scr refresh
                key_resp_vviq.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(key_resp_vviq, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'key_resp_vviq.started')
                # update status
                key_resp_vviq.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(key_resp_vviq.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(key_resp_vviq.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if key_resp_vviq.status == STARTED and not waitOnFlip:
                theseKeys = key_resp_vviq.getKeys(keyList=['1','2','3','4','5'], ignoreKeys=None, waitRelease=False)
                _key_resp_vviq_allKeys.extend(theseKeys)
                if len(_key_resp_vviq_allKeys):
                    key_resp_vviq.keys = _key_resp_vviq_allKeys[-1].name  # just the last key pressed
                    key_resp_vviq.rt = _key_resp_vviq_allKeys[-1].rt
                    key_resp_vviq.duration = _key_resp_vviq_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *vviq_task_score* updates
            
            # if vviq_task_score is starting this frame...
            if vviq_task_score.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                vviq_task_score.frameNStart = frameN  # exact frame index
                vviq_task_score.tStart = t  # local t and not account for scr refresh
                vviq_task_score.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(vviq_task_score, 'tStartRefresh')  # time at next scr refresh
                # update status
                vviq_task_score.status = STARTED
                vviq_task_score.setAutoDraw(True)
            
            # if vviq_task_score is active this frame...
            if vviq_task_score.status == STARTED:
                # update params
                pass
            
            # *vviq_task_idx* updates
            
            # if vviq_task_idx is starting this frame...
            if vviq_task_idx.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                vviq_task_idx.frameNStart = frameN  # exact frame index
                vviq_task_idx.tStart = t  # local t and not account for scr refresh
                vviq_task_idx.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(vviq_task_idx, 'tStartRefresh')  # time at next scr refresh
                # update status
                vviq_task_idx.status = STARTED
                vviq_task_idx.setAutoDraw(True)
            
            # if vviq_task_idx is active this frame...
            if vviq_task_idx.status == STARTED:
                # update params
                pass
            
            # *vviq_task_item* updates
            
            # if vviq_task_item is starting this frame...
            if vviq_task_item.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                vviq_task_item.frameNStart = frameN  # exact frame index
                vviq_task_item.tStart = t  # local t and not account for scr refresh
                vviq_task_item.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(vviq_task_item, 'tStartRefresh')  # time at next scr refresh
                # update status
                vviq_task_item.status = STARTED
                vviq_task_item.setAutoDraw(True)
            
            # if vviq_task_item is active this frame...
            if vviq_task_item.status == STARTED:
                # update params
                pass
            
            # *vviq_task_senario* updates
            
            # if vviq_task_senario is starting this frame...
            if vviq_task_senario.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                vviq_task_senario.frameNStart = frameN  # exact frame index
                vviq_task_senario.tStart = t  # local t and not account for scr refresh
                vviq_task_senario.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(vviq_task_senario, 'tStartRefresh')  # time at next scr refresh
                # update status
                vviq_task_senario.status = STARTED
                vviq_task_senario.setAutoDraw(True)
            
            # if vviq_task_senario is active this frame...
            if vviq_task_senario.status == STARTED:
                # update params
                pass
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                vviq_task.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in vviq_task.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "vviq_task" ---
        for thisComponent in vviq_task.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for vviq_task
        vviq_task.tStop = globalClock.getTime(format='float')
        vviq_task.tStopRefresh = tThisFlipGlobal
        thisExp.addData('vviq_task.stopped', vviq_task.tStop)
        # check responses
        if key_resp_vviq.keys in ['', [], None]:  # No response was made
            key_resp_vviq.keys = None
        vviq_trials.addData('key_resp_vviq.keys',key_resp_vviq.keys)
        if key_resp_vviq.keys != None:  # we had a response
            vviq_trials.addData('key_resp_vviq.rt', key_resp_vviq.rt)
            vviq_trials.addData('key_resp_vviq.duration', key_resp_vviq.duration)
        # the Routine "vviq_task" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'vviq_trials'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # --- Prepare to start Routine "vviq_feedback" ---
    # create an object to store info about Routine vviq_feedback
    vviq_feedback = data.Routine(
        name='vviq_feedback',
        components=[vviq_feedback_title, vviq_feedback_proceed, vviq_feedback_response, vviq_feedback_result],
    )
    vviq_feedback.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from vviq_feedback_text
    score1 = vviq_trials.data['key_resp_vviq.keys']
    myvviqscore = sum([int(item) for item in score1])
    if myvviqscore > 70 :
        msg = "For this vividness test, you got a total score of %i out of 80. Your vividness ability is above the general population. Congratulations! I invite you to comment on our website and write us an email at the end of the session." %(myvviqscore)
        colortext = 'green'
    elif myvviqscore >= 30:
        msg = "For this vividness test, you obtained a total score of %i out of 80. Congratulations! I invite you to comment on our post at the end of the session." %(myvviqscore)
        colortext = 'green'
    else :
        msg = "For this vividness test, you obtained a total score of %i out of 80. I invite you to contact us and do the following part which evaluates your mental image by category." %(myvviqscore)
        colortext = 'red'
    # create starting attributes for vviq_feedback_response
    vviq_feedback_response.keys = []
    vviq_feedback_response.rt = []
    _vviq_feedback_response_allKeys = []
    # store start times for vviq_feedback
    vviq_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    vviq_feedback.tStart = globalClock.getTime(format='float')
    vviq_feedback.status = STARTED
    thisExp.addData('vviq_feedback.started', vviq_feedback.tStart)
    vviq_feedback.maxDuration = None
    # keep track of which components have finished
    vviq_feedbackComponents = vviq_feedback.components
    for thisComponent in vviq_feedback.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "vviq_feedback" ---
    vviq_feedback.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *vviq_feedback_title* updates
        
        # if vviq_feedback_title is starting this frame...
        if vviq_feedback_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_feedback_title.frameNStart = frameN  # exact frame index
            vviq_feedback_title.tStart = t  # local t and not account for scr refresh
            vviq_feedback_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_feedback_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_feedback_title.status = STARTED
            vviq_feedback_title.setAutoDraw(True)
        
        # if vviq_feedback_title is active this frame...
        if vviq_feedback_title.status == STARTED:
            # update params
            pass
        
        # *vviq_feedback_proceed* updates
        
        # if vviq_feedback_proceed is starting this frame...
        if vviq_feedback_proceed.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            vviq_feedback_proceed.frameNStart = frameN  # exact frame index
            vviq_feedback_proceed.tStart = t  # local t and not account for scr refresh
            vviq_feedback_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_feedback_proceed, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_feedback_proceed.status = STARTED
            vviq_feedback_proceed.setAutoDraw(True)
        
        # if vviq_feedback_proceed is active this frame...
        if vviq_feedback_proceed.status == STARTED:
            # update params
            pass
        
        # *vviq_feedback_response* updates
        waitOnFlip = False
        
        # if vviq_feedback_response is starting this frame...
        if vviq_feedback_response.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            vviq_feedback_response.frameNStart = frameN  # exact frame index
            vviq_feedback_response.tStart = t  # local t and not account for scr refresh
            vviq_feedback_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_feedback_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_feedback_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(vviq_feedback_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(vviq_feedback_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if vviq_feedback_response.status == STARTED and not waitOnFlip:
            theseKeys = vviq_feedback_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _vviq_feedback_response_allKeys.extend(theseKeys)
            if len(_vviq_feedback_response_allKeys):
                vviq_feedback_response.keys = _vviq_feedback_response_allKeys[-1].name  # just the last key pressed
                vviq_feedback_response.rt = _vviq_feedback_response_allKeys[-1].rt
                vviq_feedback_response.duration = _vviq_feedback_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *vviq_feedback_result* updates
        
        # if vviq_feedback_result is starting this frame...
        if vviq_feedback_result.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            vviq_feedback_result.frameNStart = frameN  # exact frame index
            vviq_feedback_result.tStart = t  # local t and not account for scr refresh
            vviq_feedback_result.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(vviq_feedback_result, 'tStartRefresh')  # time at next scr refresh
            # update status
            vviq_feedback_result.status = STARTED
            vviq_feedback_result.setAutoDraw(True)
        
        # if vviq_feedback_result is active this frame...
        if vviq_feedback_result.status == STARTED:
            # update params
            vviq_feedback_result.setColor(colortext, colorSpace='rgb', log=False)
            vviq_feedback_result.setText(msg, log=False)
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            vviq_feedback.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in vviq_feedback.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "vviq_feedback" ---
    for thisComponent in vviq_feedback.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for vviq_feedback
    vviq_feedback.tStop = globalClock.getTime(format='float')
    vviq_feedback.tStopRefresh = tThisFlipGlobal
    thisExp.addData('vviq_feedback.stopped', vviq_feedback.tStop)
    thisExp.nextEntry()
    # the Routine "vviq_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "imagery_instruction" ---
    # create an object to store info about Routine imagery_instruction
    imagery_instruction = data.Routine(
        name='imagery_instruction',
        components=[imagery_instruction_title, imagery_instruction_text, imagery_instruction_proceed, imagery_instruction_proceed_2],
    )
    imagery_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for imagery_instruction_proceed_2
    imagery_instruction_proceed_2.keys = []
    imagery_instruction_proceed_2.rt = []
    _imagery_instruction_proceed_2_allKeys = []
    # store start times for imagery_instruction
    imagery_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    imagery_instruction.tStart = globalClock.getTime(format='float')
    imagery_instruction.status = STARTED
    thisExp.addData('imagery_instruction.started', imagery_instruction.tStart)
    imagery_instruction.maxDuration = None
    # keep track of which components have finished
    imagery_instructionComponents = imagery_instruction.components
    for thisComponent in imagery_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "imagery_instruction" ---
    imagery_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *imagery_instruction_title* updates
        
        # if imagery_instruction_title is starting this frame...
        if imagery_instruction_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            imagery_instruction_title.frameNStart = frameN  # exact frame index
            imagery_instruction_title.tStart = t  # local t and not account for scr refresh
            imagery_instruction_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imagery_instruction_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            imagery_instruction_title.status = STARTED
            imagery_instruction_title.setAutoDraw(True)
        
        # if imagery_instruction_title is active this frame...
        if imagery_instruction_title.status == STARTED:
            # update params
            pass
        
        # *imagery_instruction_text* updates
        
        # if imagery_instruction_text is starting this frame...
        if imagery_instruction_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            imagery_instruction_text.frameNStart = frameN  # exact frame index
            imagery_instruction_text.tStart = t  # local t and not account for scr refresh
            imagery_instruction_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imagery_instruction_text, 'tStartRefresh')  # time at next scr refresh
            # update status
            imagery_instruction_text.status = STARTED
            imagery_instruction_text.setAutoDraw(True)
        
        # if imagery_instruction_text is active this frame...
        if imagery_instruction_text.status == STARTED:
            # update params
            pass
        
        # *imagery_instruction_proceed* updates
        
        # if imagery_instruction_proceed is starting this frame...
        if imagery_instruction_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            imagery_instruction_proceed.frameNStart = frameN  # exact frame index
            imagery_instruction_proceed.tStart = t  # local t and not account for scr refresh
            imagery_instruction_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imagery_instruction_proceed, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'imagery_instruction_proceed.started')
            # update status
            imagery_instruction_proceed.status = STARTED
            imagery_instruction_proceed.setAutoDraw(True)
        
        # if imagery_instruction_proceed is active this frame...
        if imagery_instruction_proceed.status == STARTED:
            # update params
            pass
        
        # *imagery_instruction_proceed_2* updates
        waitOnFlip = False
        
        # if imagery_instruction_proceed_2 is starting this frame...
        if imagery_instruction_proceed_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            imagery_instruction_proceed_2.frameNStart = frameN  # exact frame index
            imagery_instruction_proceed_2.tStart = t  # local t and not account for scr refresh
            imagery_instruction_proceed_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imagery_instruction_proceed_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            imagery_instruction_proceed_2.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(imagery_instruction_proceed_2.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(imagery_instruction_proceed_2.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if imagery_instruction_proceed_2.status == STARTED and not waitOnFlip:
            theseKeys = imagery_instruction_proceed_2.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _imagery_instruction_proceed_2_allKeys.extend(theseKeys)
            if len(_imagery_instruction_proceed_2_allKeys):
                imagery_instruction_proceed_2.keys = _imagery_instruction_proceed_2_allKeys[-1].name  # just the last key pressed
                imagery_instruction_proceed_2.rt = _imagery_instruction_proceed_2_allKeys[-1].rt
                imagery_instruction_proceed_2.duration = _imagery_instruction_proceed_2_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            imagery_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in imagery_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "imagery_instruction" ---
    for thisComponent in imagery_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for imagery_instruction
    imagery_instruction.tStop = globalClock.getTime(format='float')
    imagery_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('imagery_instruction.stopped', imagery_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "imagery_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    imagery_domain = data.TrialHandler2(
        name='imagery_domain',
        nReps=1.0, 
        method='random', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions('Imagery_domain_selection.xlsx'), 
        seed=None, 
    )
    thisExp.addLoop(imagery_domain)  # add the loop to the experiment
    thisImagery_domain = imagery_domain.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisImagery_domain.rgb)
    if thisImagery_domain != None:
        for paramName in thisImagery_domain:
            globals()[paramName] = thisImagery_domain[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisImagery_domain in imagery_domain:
        currentLoop = imagery_domain
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisImagery_domain.rgb)
        if thisImagery_domain != None:
            for paramName in thisImagery_domain:
                globals()[paramName] = thisImagery_domain[paramName]
        
        # set up handler to look after randomisation of conditions etc
        imagery_trials = data.TrialHandler2(
            name='imagery_trials',
            nReps=1.0, 
            method='sequential', 
            extraInfo=expInfo, 
            originPath=-1, 
            trialList=data.importConditions(
            'ImageryPractice_en.xlsx', 
            selection=practice_idx
        )
        , 
            seed=None, 
        )
        thisExp.addLoop(imagery_trials)  # add the loop to the experiment
        thisImagery_trial = imagery_trials.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisImagery_trial.rgb)
        if thisImagery_trial != None:
            for paramName in thisImagery_trial:
                globals()[paramName] = thisImagery_trial[paramName]
        
        for thisImagery_trial in imagery_trials:
            currentLoop = imagery_trials
            thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
            # abbreviate parameter names if possible (e.g. rgb = thisImagery_trial.rgb)
            if thisImagery_trial != None:
                for paramName in thisImagery_trial:
                    globals()[paramName] = thisImagery_trial[paramName]
            
            # --- Prepare to start Routine "imagery_task_trials" ---
            # create an object to store info about Routine imagery_task_trials
            imagery_task_trials = data.Routine(
                name='imagery_task_trials',
                components=[imagery_task_trials_question, imagery_task_trials_word1, imagery_task_trials_word2, imagery_task_trials_instruction, imagery_task_trials_response, imagery_task_trials_order_instruction, imagery_task_trials_img1, imagery_task_trials_proceed],
            )
            imagery_task_trials.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            imagery_task_trials_question.setText(task_question)
            imagery_task_trials_word1.setSound(word1, secs=2, hamming=False)
            imagery_task_trials_word1.setVolume(8, log=False)
            imagery_task_trials_word1.seek(0)
            imagery_task_trials_word2.setSound(word2, secs=2, hamming=False)
            imagery_task_trials_word2.setVolume(8, log=False)
            imagery_task_trials_word2.seek(0)
            imagery_task_trials_instruction.setSound(instruction, hamming=False)
            imagery_task_trials_instruction.setVolume(8, log=False)
            imagery_task_trials_instruction.seek(0)
            # create starting attributes for imagery_task_trials_response
            imagery_task_trials_response.keys = []
            imagery_task_trials_response.rt = []
            _imagery_task_trials_response_allKeys = []
            imagery_task_trials_order_instruction.setText(order_instruction)
            imagery_task_trials_img1.setImage(img1)
            imagery_task_trials_proceed.setText(task_continue)
            # store start times for imagery_task_trials
            imagery_task_trials.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            imagery_task_trials.tStart = globalClock.getTime(format='float')
            imagery_task_trials.status = STARTED
            thisExp.addData('imagery_task_trials.started', imagery_task_trials.tStart)
            imagery_task_trials.maxDuration = None
            # keep track of which components have finished
            imagery_task_trialsComponents = imagery_task_trials.components
            for thisComponent in imagery_task_trials.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "imagery_task_trials" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_trials, data.TrialHandler2) and thisImagery_trial.thisN != imagery_trials.thisTrial.thisN:
                continueRoutine = False
            imagery_task_trials.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_trials_question* updates
                
                # if imagery_task_trials_question is starting this frame...
                if imagery_task_trials_question.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_question.frameNStart = frameN  # exact frame index
                    imagery_task_trials_question.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_question.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_question, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_question.status = STARTED
                    imagery_task_trials_question.setAutoDraw(True)
                
                # if imagery_task_trials_question is active this frame...
                if imagery_task_trials_question.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_trials_word1* updates
                
                # if imagery_task_trials_word1 is starting this frame...
                if imagery_task_trials_word1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_word1.frameNStart = frameN  # exact frame index
                    imagery_task_trials_word1.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_word1.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_trials_word1.status = STARTED
                    imagery_task_trials_word1.play(when=win)  # sync with win flip
                
                # if imagery_task_trials_word1 is stopping this frame...
                if imagery_task_trials_word1.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_trials_word1.tStartRefresh + 2-frameTolerance or imagery_task_trials_word1.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_trials_word1.tStop = t  # not accounting for scr refresh
                        imagery_task_trials_word1.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_trials_word1.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_trials_word1.status = FINISHED
                        imagery_task_trials_word1.stop()
                
                # *imagery_task_trials_word2* updates
                
                # if imagery_task_trials_word2 is starting this frame...
                if imagery_task_trials_word2.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_word2.frameNStart = frameN  # exact frame index
                    imagery_task_trials_word2.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_word2.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_trials_word2.status = STARTED
                    imagery_task_trials_word2.play(when=win)  # sync with win flip
                
                # if imagery_task_trials_word2 is stopping this frame...
                if imagery_task_trials_word2.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_trials_word2.tStartRefresh + 2-frameTolerance or imagery_task_trials_word2.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_trials_word2.tStop = t  # not accounting for scr refresh
                        imagery_task_trials_word2.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_trials_word2.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_trials_word2.status = FINISHED
                        imagery_task_trials_word2.stop()
                
                # *imagery_task_trials_instruction* updates
                
                # if imagery_task_trials_instruction is starting this frame...
                if imagery_task_trials_instruction.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_instruction.frameNStart = frameN  # exact frame index
                    imagery_task_trials_instruction.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_trials_instruction.status = STARTED
                    imagery_task_trials_instruction.play(when=win)  # sync with win flip
                
                # if imagery_task_trials_instruction is stopping this frame...
                if imagery_task_trials_instruction.status == STARTED:
                    if bool(False) or imagery_task_trials_instruction.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_trials_instruction.tStop = t  # not accounting for scr refresh
                        imagery_task_trials_instruction.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_trials_instruction.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_trials_instruction.status = FINISHED
                        imagery_task_trials_instruction.stop()
                
                # *imagery_task_trials_response* updates
                waitOnFlip = False
                
                # if imagery_task_trials_response is starting this frame...
                if imagery_task_trials_response.status == NOT_STARTED and imagery_task_trials_instruction.status==FINISHED:
                    # keep track of start time/frame for later
                    imagery_task_trials_response.frameNStart = frameN  # exact frame index
                    imagery_task_trials_response.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_response.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_response, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_response.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(imagery_task_trials_response.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(imagery_task_trials_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if imagery_task_trials_response.status == STARTED and not waitOnFlip:
                    theseKeys = imagery_task_trials_response.getKeys(keyList=['1','2'], ignoreKeys=None, waitRelease=False)
                    _imagery_task_trials_response_allKeys.extend(theseKeys)
                    if len(_imagery_task_trials_response_allKeys):
                        imagery_task_trials_response.keys = _imagery_task_trials_response_allKeys[-1].name  # just the last key pressed
                        imagery_task_trials_response.rt = _imagery_task_trials_response_allKeys[-1].rt
                        imagery_task_trials_response.duration = _imagery_task_trials_response_allKeys[-1].duration
                        # a response ends the routine
                        continueRoutine = False
                
                # *imagery_task_trials_order_instruction* updates
                
                # if imagery_task_trials_order_instruction is starting this frame...
                if imagery_task_trials_order_instruction.status == NOT_STARTED and tThisFlip >= 4.5-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_order_instruction.frameNStart = frameN  # exact frame index
                    imagery_task_trials_order_instruction.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_order_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_order_instruction, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_order_instruction.status = STARTED
                    imagery_task_trials_order_instruction.setAutoDraw(True)
                
                # if imagery_task_trials_order_instruction is active this frame...
                if imagery_task_trials_order_instruction.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_trials_img1* updates
                
                # if imagery_task_trials_img1 is starting this frame...
                if imagery_task_trials_img1.status == NOT_STARTED and tThisFlip >= 4.5-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_img1.frameNStart = frameN  # exact frame index
                    imagery_task_trials_img1.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_img1.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_img1, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_img1.status = STARTED
                    imagery_task_trials_img1.setAutoDraw(True)
                
                # if imagery_task_trials_img1 is active this frame...
                if imagery_task_trials_img1.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_trials_proceed* updates
                
                # if imagery_task_trials_proceed is starting this frame...
                if imagery_task_trials_proceed.status == NOT_STARTED and tThisFlip >= 4.5-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_proceed.frameNStart = frameN  # exact frame index
                    imagery_task_trials_proceed.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_proceed.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_proceed, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_proceed.status = STARTED
                    imagery_task_trials_proceed.setAutoDraw(True)
                
                # if imagery_task_trials_proceed is active this frame...
                if imagery_task_trials_proceed.status == STARTED:
                    # update params
                    pass
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[imagery_task_trials_word1, imagery_task_trials_word2, imagery_task_trials_instruction]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    imagery_task_trials.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in imagery_task_trials.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "imagery_task_trials" ---
            for thisComponent in imagery_task_trials.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for imagery_task_trials
            imagery_task_trials.tStop = globalClock.getTime(format='float')
            imagery_task_trials.tStopRefresh = tThisFlipGlobal
            thisExp.addData('imagery_task_trials.stopped', imagery_task_trials.tStop)
            imagery_task_trials_word1.pause()  # ensure sound has stopped at end of Routine
            imagery_task_trials_word2.pause()  # ensure sound has stopped at end of Routine
            imagery_task_trials_instruction.pause()  # ensure sound has stopped at end of Routine
            # the Routine "imagery_task_trials" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "imagery_task_trials_vividness" ---
            # create an object to store info about Routine imagery_task_trials_vividness
            imagery_task_trials_vividness = data.Routine(
                name='imagery_task_trials_vividness',
                components=[imagery_task_trials_vividness_question, imagery_task_trials_vividness_text, imagery_task_trials_vividness_response],
            )
            imagery_task_trials_vividness.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            imagery_task_trials_vividness_question.setText(task_question)
            imagery_task_trials_vividness_text.setText(task_vividness)
            # create starting attributes for imagery_task_trials_vividness_response
            imagery_task_trials_vividness_response.keys = []
            imagery_task_trials_vividness_response.rt = []
            _imagery_task_trials_vividness_response_allKeys = []
            # store start times for imagery_task_trials_vividness
            imagery_task_trials_vividness.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            imagery_task_trials_vividness.tStart = globalClock.getTime(format='float')
            imagery_task_trials_vividness.status = STARTED
            thisExp.addData('imagery_task_trials_vividness.started', imagery_task_trials_vividness.tStart)
            imagery_task_trials_vividness.maxDuration = None
            # keep track of which components have finished
            imagery_task_trials_vividnessComponents = imagery_task_trials_vividness.components
            for thisComponent in imagery_task_trials_vividness.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "imagery_task_trials_vividness" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_trials, data.TrialHandler2) and thisImagery_trial.thisN != imagery_trials.thisTrial.thisN:
                continueRoutine = False
            imagery_task_trials_vividness.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_trials_vividness_question* updates
                
                # if imagery_task_trials_vividness_question is starting this frame...
                if imagery_task_trials_vividness_question.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_vividness_question.frameNStart = frameN  # exact frame index
                    imagery_task_trials_vividness_question.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_vividness_question.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_vividness_question, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_vividness_question.status = STARTED
                    imagery_task_trials_vividness_question.setAutoDraw(True)
                
                # if imagery_task_trials_vividness_question is active this frame...
                if imagery_task_trials_vividness_question.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_trials_vividness_text* updates
                
                # if imagery_task_trials_vividness_text is starting this frame...
                if imagery_task_trials_vividness_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_vividness_text.frameNStart = frameN  # exact frame index
                    imagery_task_trials_vividness_text.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_vividness_text.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_vividness_text, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_vividness_text.status = STARTED
                    imagery_task_trials_vividness_text.setAutoDraw(True)
                
                # if imagery_task_trials_vividness_text is active this frame...
                if imagery_task_trials_vividness_text.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_trials_vividness_response* updates
                waitOnFlip = False
                
                # if imagery_task_trials_vividness_response is starting this frame...
                if imagery_task_trials_vividness_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_vividness_response.frameNStart = frameN  # exact frame index
                    imagery_task_trials_vividness_response.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_vividness_response.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_vividness_response, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_trials_vividness_response.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(imagery_task_trials_vividness_response.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(imagery_task_trials_vividness_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if imagery_task_trials_vividness_response.status == STARTED and not waitOnFlip:
                    theseKeys = imagery_task_trials_vividness_response.getKeys(keyList=['1','2','3','4'], ignoreKeys=None, waitRelease=False)
                    _imagery_task_trials_vividness_response_allKeys.extend(theseKeys)
                    if len(_imagery_task_trials_vividness_response_allKeys):
                        imagery_task_trials_vividness_response.keys = _imagery_task_trials_vividness_response_allKeys[-1].name  # just the last key pressed
                        imagery_task_trials_vividness_response.rt = _imagery_task_trials_vividness_response_allKeys[-1].rt
                        imagery_task_trials_vividness_response.duration = _imagery_task_trials_vividness_response_allKeys[-1].duration
                        # a response ends the routine
                        continueRoutine = False
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    imagery_task_trials_vividness.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in imagery_task_trials_vividness.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "imagery_task_trials_vividness" ---
            for thisComponent in imagery_task_trials_vividness.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for imagery_task_trials_vividness
            imagery_task_trials_vividness.tStop = globalClock.getTime(format='float')
            imagery_task_trials_vividness.tStopRefresh = tThisFlipGlobal
            thisExp.addData('imagery_task_trials_vividness.stopped', imagery_task_trials_vividness.tStop)
            # the Routine "imagery_task_trials_vividness" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "resting" ---
            # create an object to store info about Routine resting
            resting = data.Routine(
                name='resting',
                components=[imagery_task_trials_rest],
            )
            resting.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # store start times for resting
            resting.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            resting.tStart = globalClock.getTime(format='float')
            resting.status = STARTED
            thisExp.addData('resting.started', resting.tStart)
            resting.maxDuration = None
            # keep track of which components have finished
            restingComponents = resting.components
            for thisComponent in resting.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "resting" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_trials, data.TrialHandler2) and thisImagery_trial.thisN != imagery_trials.thisTrial.thisN:
                continueRoutine = False
            resting.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine and routineTimer.getTime() < 0.5:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_trials_rest* updates
                
                # if imagery_task_trials_rest is starting this frame...
                if imagery_task_trials_rest.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_rest.frameNStart = frameN  # exact frame index
                    imagery_task_trials_rest.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_rest.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_rest, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.started')
                    # update status
                    imagery_task_trials_rest.status = STARTED
                    imagery_task_trials_rest.setAutoDraw(True)
                
                # if imagery_task_trials_rest is active this frame...
                if imagery_task_trials_rest.status == STARTED:
                    # update params
                    pass
                
                # if imagery_task_trials_rest is stopping this frame...
                if imagery_task_trials_rest.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_trials_rest.tStartRefresh + 0.5-frameTolerance:
                        # keep track of stop time/frame for later
                        imagery_task_trials_rest.tStop = t  # not accounting for scr refresh
                        imagery_task_trials_rest.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_trials_rest.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.stopped')
                        # update status
                        imagery_task_trials_rest.status = FINISHED
                        imagery_task_trials_rest.setAutoDraw(False)
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    resting.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in resting.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "resting" ---
            for thisComponent in resting.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for resting
            resting.tStop = globalClock.getTime(format='float')
            resting.tStopRefresh = tThisFlipGlobal
            thisExp.addData('resting.stopped', resting.tStop)
            # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
            if resting.maxDurationReached:
                routineTimer.addTime(-resting.maxDuration)
            elif resting.forceEnded:
                routineTimer.reset()
            else:
                routineTimer.addTime(-0.500000)
        # completed 1.0 repeats of 'imagery_trials'
        
        
        # --- Prepare to start Routine "imagery_task_prepare" ---
        # create an object to store info about Routine imagery_task_prepare
        imagery_task_prepare = data.Routine(
            name='imagery_task_prepare',
            components=[imagery_task_prepare_title, imagery_task_prepare_text, imagery_task_prepare_response, imagery_task_prepare_proceed],
        )
        imagery_task_prepare.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # create starting attributes for imagery_task_prepare_response
        imagery_task_prepare_response.keys = []
        imagery_task_prepare_response.rt = []
        _imagery_task_prepare_response_allKeys = []
        # store start times for imagery_task_prepare
        imagery_task_prepare.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        imagery_task_prepare.tStart = globalClock.getTime(format='float')
        imagery_task_prepare.status = STARTED
        thisExp.addData('imagery_task_prepare.started', imagery_task_prepare.tStart)
        imagery_task_prepare.maxDuration = None
        # keep track of which components have finished
        imagery_task_prepareComponents = imagery_task_prepare.components
        for thisComponent in imagery_task_prepare.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "imagery_task_prepare" ---
        # if trial has changed, end Routine now
        if isinstance(imagery_domain, data.TrialHandler2) and thisImagery_domain.thisN != imagery_domain.thisTrial.thisN:
            continueRoutine = False
        imagery_task_prepare.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *imagery_task_prepare_title* updates
            
            # if imagery_task_prepare_title is starting this frame...
            if imagery_task_prepare_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_prepare_title.frameNStart = frameN  # exact frame index
                imagery_task_prepare_title.tStart = t  # local t and not account for scr refresh
                imagery_task_prepare_title.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_prepare_title, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_prepare_title.status = STARTED
                imagery_task_prepare_title.setAutoDraw(True)
            
            # if imagery_task_prepare_title is active this frame...
            if imagery_task_prepare_title.status == STARTED:
                # update params
                pass
            
            # *imagery_task_prepare_text* updates
            
            # if imagery_task_prepare_text is starting this frame...
            if imagery_task_prepare_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_prepare_text.frameNStart = frameN  # exact frame index
                imagery_task_prepare_text.tStart = t  # local t and not account for scr refresh
                imagery_task_prepare_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_prepare_text, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_prepare_text.status = STARTED
                imagery_task_prepare_text.setAutoDraw(True)
            
            # if imagery_task_prepare_text is active this frame...
            if imagery_task_prepare_text.status == STARTED:
                # update params
                pass
            
            # *imagery_task_prepare_response* updates
            waitOnFlip = False
            
            # if imagery_task_prepare_response is starting this frame...
            if imagery_task_prepare_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_prepare_response.frameNStart = frameN  # exact frame index
                imagery_task_prepare_response.tStart = t  # local t and not account for scr refresh
                imagery_task_prepare_response.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_prepare_response, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_prepare_response.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(imagery_task_prepare_response.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(imagery_task_prepare_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if imagery_task_prepare_response.status == STARTED and not waitOnFlip:
                theseKeys = imagery_task_prepare_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
                _imagery_task_prepare_response_allKeys.extend(theseKeys)
                if len(_imagery_task_prepare_response_allKeys):
                    imagery_task_prepare_response.keys = _imagery_task_prepare_response_allKeys[-1].name  # just the last key pressed
                    imagery_task_prepare_response.rt = _imagery_task_prepare_response_allKeys[-1].rt
                    imagery_task_prepare_response.duration = _imagery_task_prepare_response_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *imagery_task_prepare_proceed* updates
            
            # if imagery_task_prepare_proceed is starting this frame...
            if imagery_task_prepare_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_prepare_proceed.frameNStart = frameN  # exact frame index
                imagery_task_prepare_proceed.tStart = t  # local t and not account for scr refresh
                imagery_task_prepare_proceed.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_prepare_proceed, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_prepare_proceed.status = STARTED
                imagery_task_prepare_proceed.setAutoDraw(True)
            
            # if imagery_task_prepare_proceed is active this frame...
            if imagery_task_prepare_proceed.status == STARTED:
                # update params
                pass
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                imagery_task_prepare.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in imagery_task_prepare.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "imagery_task_prepare" ---
        for thisComponent in imagery_task_prepare.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for imagery_task_prepare
        imagery_task_prepare.tStop = globalClock.getTime(format='float')
        imagery_task_prepare.tStopRefresh = tThisFlipGlobal
        thisExp.addData('imagery_task_prepare.stopped', imagery_task_prepare.tStop)
        # the Routine "imagery_task_prepare" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # set up handler to look after randomisation of conditions etc
        imagery_task_items = data.TrialHandler2(
            name='imagery_task_items',
            nReps=1.0, 
            method='random', 
            extraInfo=expInfo, 
            originPath=-1, 
            trialList=data.importConditions(
            'ImageryItems_en.xlsx', 
            selection=trialRow
        )
        , 
            seed=None, 
        )
        thisExp.addLoop(imagery_task_items)  # add the loop to the experiment
        thisImagery_task_item = imagery_task_items.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisImagery_task_item.rgb)
        if thisImagery_task_item != None:
            for paramName in thisImagery_task_item:
                globals()[paramName] = thisImagery_task_item[paramName]
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        
        for thisImagery_task_item in imagery_task_items:
            currentLoop = imagery_task_items
            thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
            if thisSession is not None:
                # if running in a Session with a Liaison client, send data up to now
                thisSession.sendExperimentData()
            # abbreviate parameter names if possible (e.g. rgb = thisImagery_task_item.rgb)
            if thisImagery_task_item != None:
                for paramName in thisImagery_task_item:
                    globals()[paramName] = thisImagery_task_item[paramName]
            
            # --- Prepare to start Routine "imagery_task" ---
            # create an object to store info about Routine imagery_task
            imagery_task = data.Routine(
                name='imagery_task',
                components=[imagery_task_title, imagery_task_idx, imagery_task_word1, imagery_task_word2, imagery_task_instruction, imagery_task_text, key_resp_vmi],
            )
            imagery_task.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            imagery_task_idx.setText(imagery_task_items.thisN)
            imagery_task_word1.setSound(word1, secs=2, hamming=False)
            imagery_task_word1.setVolume(8, log=False)
            imagery_task_word1.seek(0)
            imagery_task_word2.setSound(word2, secs=2, hamming=False)
            imagery_task_word2.setVolume(8, log=False)
            imagery_task_word2.seek(0)
            imagery_task_instruction.setSound(instruction, hamming=False)
            imagery_task_instruction.setVolume(8, log=False)
            imagery_task_instruction.seek(0)
            imagery_task_text.setText('Give your answer: first (key "1") or second (key "2").')
            # create starting attributes for key_resp_vmi
            key_resp_vmi.keys = []
            key_resp_vmi.rt = []
            _key_resp_vmi_allKeys = []
            # store start times for imagery_task
            imagery_task.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            imagery_task.tStart = globalClock.getTime(format='float')
            imagery_task.status = STARTED
            thisExp.addData('imagery_task.started', imagery_task.tStart)
            imagery_task.maxDuration = None
            # keep track of which components have finished
            imagery_taskComponents = imagery_task.components
            for thisComponent in imagery_task.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "imagery_task" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_task_items, data.TrialHandler2) and thisImagery_task_item.thisN != imagery_task_items.thisTrial.thisN:
                continueRoutine = False
            imagery_task.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_title* updates
                
                # if imagery_task_title is starting this frame...
                if imagery_task_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_title.frameNStart = frameN  # exact frame index
                    imagery_task_title.tStart = t  # local t and not account for scr refresh
                    imagery_task_title.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_title, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_title.status = STARTED
                    imagery_task_title.setAutoDraw(True)
                
                # if imagery_task_title is active this frame...
                if imagery_task_title.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_idx* updates
                
                # if imagery_task_idx is starting this frame...
                if imagery_task_idx.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_idx.frameNStart = frameN  # exact frame index
                    imagery_task_idx.tStart = t  # local t and not account for scr refresh
                    imagery_task_idx.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_idx, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_idx.status = STARTED
                    imagery_task_idx.setAutoDraw(True)
                
                # if imagery_task_idx is active this frame...
                if imagery_task_idx.status == STARTED:
                    # update params
                    pass
                
                # *imagery_task_word1* updates
                
                # if imagery_task_word1 is starting this frame...
                if imagery_task_word1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_word1.frameNStart = frameN  # exact frame index
                    imagery_task_word1.tStart = t  # local t and not account for scr refresh
                    imagery_task_word1.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_word1.status = STARTED
                    imagery_task_word1.play(when=win)  # sync with win flip
                
                # if imagery_task_word1 is stopping this frame...
                if imagery_task_word1.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_word1.tStartRefresh + 2-frameTolerance or imagery_task_word1.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_word1.tStop = t  # not accounting for scr refresh
                        imagery_task_word1.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_word1.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_word1.status = FINISHED
                        imagery_task_word1.stop()
                
                # *imagery_task_word2* updates
                
                # if imagery_task_word2 is starting this frame...
                if imagery_task_word2.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_word2.frameNStart = frameN  # exact frame index
                    imagery_task_word2.tStart = t  # local t and not account for scr refresh
                    imagery_task_word2.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_word2.status = STARTED
                    imagery_task_word2.play(when=win)  # sync with win flip
                
                # if imagery_task_word2 is stopping this frame...
                if imagery_task_word2.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_word2.tStartRefresh + 2-frameTolerance or imagery_task_word2.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_word2.tStop = t  # not accounting for scr refresh
                        imagery_task_word2.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_word2.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_word2.status = FINISHED
                        imagery_task_word2.stop()
                
                # *imagery_task_instruction* updates
                
                # if imagery_task_instruction is starting this frame...
                if imagery_task_instruction.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_instruction.frameNStart = frameN  # exact frame index
                    imagery_task_instruction.tStart = t  # local t and not account for scr refresh
                    imagery_task_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                    # update status
                    imagery_task_instruction.status = STARTED
                    imagery_task_instruction.play(when=win)  # sync with win flip
                
                # if imagery_task_instruction is stopping this frame...
                if imagery_task_instruction.status == STARTED:
                    if bool(False) or imagery_task_instruction.isFinished:
                        # keep track of stop time/frame for later
                        imagery_task_instruction.tStop = t  # not accounting for scr refresh
                        imagery_task_instruction.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_instruction.frameNStop = frameN  # exact frame index
                        # update status
                        imagery_task_instruction.status = FINISHED
                        imagery_task_instruction.stop()
                
                # *imagery_task_text* updates
                
                # if imagery_task_text is starting this frame...
                if imagery_task_text.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_text.frameNStart = frameN  # exact frame index
                    imagery_task_text.tStart = t  # local t and not account for scr refresh
                    imagery_task_text.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_text, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_text.status = STARTED
                    imagery_task_text.setAutoDraw(True)
                
                # if imagery_task_text is active this frame...
                if imagery_task_text.status == STARTED:
                    # update params
                    pass
                
                # *key_resp_vmi* updates
                waitOnFlip = False
                
                # if key_resp_vmi is starting this frame...
                if key_resp_vmi.status == NOT_STARTED and imagery_task_instruction.status==FINISHED:
                    # keep track of start time/frame for later
                    key_resp_vmi.frameNStart = frameN  # exact frame index
                    key_resp_vmi.tStart = t  # local t and not account for scr refresh
                    key_resp_vmi.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(key_resp_vmi, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'key_resp_vmi.started')
                    # update status
                    key_resp_vmi.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(key_resp_vmi.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(key_resp_vmi.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if key_resp_vmi.status == STARTED and not waitOnFlip:
                    theseKeys = key_resp_vmi.getKeys(keyList=['1','2'], ignoreKeys=None, waitRelease=False)
                    _key_resp_vmi_allKeys.extend(theseKeys)
                    if len(_key_resp_vmi_allKeys):
                        key_resp_vmi.keys = _key_resp_vmi_allKeys[-1].name  # just the last key pressed
                        key_resp_vmi.rt = _key_resp_vmi_allKeys[-1].rt
                        key_resp_vmi.duration = _key_resp_vmi_allKeys[-1].duration
                        # was this correct?
                        if (key_resp_vmi.keys == str(correctAns)) or (key_resp_vmi.keys == correctAns):
                            key_resp_vmi.corr = 1
                        else:
                            key_resp_vmi.corr = 0
                        # a response ends the routine
                        continueRoutine = False
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[imagery_task_word1, imagery_task_word2, imagery_task_instruction]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    imagery_task.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in imagery_task.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "imagery_task" ---
            for thisComponent in imagery_task.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for imagery_task
            imagery_task.tStop = globalClock.getTime(format='float')
            imagery_task.tStopRefresh = tThisFlipGlobal
            thisExp.addData('imagery_task.stopped', imagery_task.tStop)
            imagery_task_word1.pause()  # ensure sound has stopped at end of Routine
            imagery_task_word2.pause()  # ensure sound has stopped at end of Routine
            imagery_task_instruction.pause()  # ensure sound has stopped at end of Routine
            # check responses
            if key_resp_vmi.keys in ['', [], None]:  # No response was made
                key_resp_vmi.keys = None
                # was no response the correct answer?!
                if str(correctAns).lower() == 'none':
                   key_resp_vmi.corr = 1;  # correct non-response
                else:
                   key_resp_vmi.corr = 0;  # failed to respond (incorrectly)
            # store data for imagery_task_items (TrialHandler)
            imagery_task_items.addData('key_resp_vmi.keys',key_resp_vmi.keys)
            imagery_task_items.addData('key_resp_vmi.corr', key_resp_vmi.corr)
            if key_resp_vmi.keys != None:  # we had a response
                imagery_task_items.addData('key_resp_vmi.rt', key_resp_vmi.rt)
                imagery_task_items.addData('key_resp_vmi.duration', key_resp_vmi.duration)
            # the Routine "imagery_task" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "imagery_task_vividness" ---
            # create an object to store info about Routine imagery_task_vividness
            imagery_task_vividness = data.Routine(
                name='imagery_task_vividness',
                components=[imagery_task_vividness_title, vividness],
            )
            imagery_task_vividness.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # create starting attributes for vividness
            vividness.keys = []
            vividness.rt = []
            _vividness_allKeys = []
            # store start times for imagery_task_vividness
            imagery_task_vividness.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            imagery_task_vividness.tStart = globalClock.getTime(format='float')
            imagery_task_vividness.status = STARTED
            thisExp.addData('imagery_task_vividness.started', imagery_task_vividness.tStart)
            imagery_task_vividness.maxDuration = None
            # keep track of which components have finished
            imagery_task_vividnessComponents = imagery_task_vividness.components
            for thisComponent in imagery_task_vividness.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "imagery_task_vividness" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_task_items, data.TrialHandler2) and thisImagery_task_item.thisN != imagery_task_items.thisTrial.thisN:
                continueRoutine = False
            imagery_task_vividness.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_vividness_title* updates
                
                # if imagery_task_vividness_title is starting this frame...
                if imagery_task_vividness_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_vividness_title.frameNStart = frameN  # exact frame index
                    imagery_task_vividness_title.tStart = t  # local t and not account for scr refresh
                    imagery_task_vividness_title.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_vividness_title, 'tStartRefresh')  # time at next scr refresh
                    # update status
                    imagery_task_vividness_title.status = STARTED
                    imagery_task_vividness_title.setAutoDraw(True)
                
                # if imagery_task_vividness_title is active this frame...
                if imagery_task_vividness_title.status == STARTED:
                    # update params
                    pass
                
                # *vividness* updates
                waitOnFlip = False
                
                # if vividness is starting this frame...
                if vividness.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    vividness.frameNStart = frameN  # exact frame index
                    vividness.tStart = t  # local t and not account for scr refresh
                    vividness.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(vividness, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'vividness.started')
                    # update status
                    vividness.status = STARTED
                    # keyboard checking is just starting
                    waitOnFlip = True
                    win.callOnFlip(vividness.clock.reset)  # t=0 on next screen flip
                    win.callOnFlip(vividness.clearEvents, eventType='keyboard')  # clear events on next screen flip
                if vividness.status == STARTED and not waitOnFlip:
                    theseKeys = vividness.getKeys(keyList=['1','2','3','4'], ignoreKeys=None, waitRelease=False)
                    _vividness_allKeys.extend(theseKeys)
                    if len(_vividness_allKeys):
                        vividness.keys = _vividness_allKeys[-1].name  # just the last key pressed
                        vividness.rt = _vividness_allKeys[-1].rt
                        vividness.duration = _vividness_allKeys[-1].duration
                        # a response ends the routine
                        continueRoutine = False
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    imagery_task_vividness.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in imagery_task_vividness.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "imagery_task_vividness" ---
            for thisComponent in imagery_task_vividness.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for imagery_task_vividness
            imagery_task_vividness.tStop = globalClock.getTime(format='float')
            imagery_task_vividness.tStopRefresh = tThisFlipGlobal
            thisExp.addData('imagery_task_vividness.stopped', imagery_task_vividness.tStop)
            # check responses
            if vividness.keys in ['', [], None]:  # No response was made
                vividness.keys = None
            imagery_task_items.addData('vividness.keys',vividness.keys)
            if vividness.keys != None:  # we had a response
                imagery_task_items.addData('vividness.rt', vividness.rt)
                imagery_task_items.addData('vividness.duration', vividness.duration)
            # the Routine "imagery_task_vividness" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "resting" ---
            # create an object to store info about Routine resting
            resting = data.Routine(
                name='resting',
                components=[imagery_task_trials_rest],
            )
            resting.status = NOT_STARTED
            continueRoutine = True
            # update component parameters for each repeat
            # store start times for resting
            resting.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
            resting.tStart = globalClock.getTime(format='float')
            resting.status = STARTED
            thisExp.addData('resting.started', resting.tStart)
            resting.maxDuration = None
            # keep track of which components have finished
            restingComponents = resting.components
            for thisComponent in resting.components:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "resting" ---
            # if trial has changed, end Routine now
            if isinstance(imagery_task_items, data.TrialHandler2) and thisImagery_task_item.thisN != imagery_task_items.thisTrial.thisN:
                continueRoutine = False
            resting.forceEnded = routineForceEnded = not continueRoutine
            while continueRoutine and routineTimer.getTime() < 0.5:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *imagery_task_trials_rest* updates
                
                # if imagery_task_trials_rest is starting this frame...
                if imagery_task_trials_rest.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    imagery_task_trials_rest.frameNStart = frameN  # exact frame index
                    imagery_task_trials_rest.tStart = t  # local t and not account for scr refresh
                    imagery_task_trials_rest.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(imagery_task_trials_rest, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.started')
                    # update status
                    imagery_task_trials_rest.status = STARTED
                    imagery_task_trials_rest.setAutoDraw(True)
                
                # if imagery_task_trials_rest is active this frame...
                if imagery_task_trials_rest.status == STARTED:
                    # update params
                    pass
                
                # if imagery_task_trials_rest is stopping this frame...
                if imagery_task_trials_rest.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > imagery_task_trials_rest.tStartRefresh + 0.5-frameTolerance:
                        # keep track of stop time/frame for later
                        imagery_task_trials_rest.tStop = t  # not accounting for scr refresh
                        imagery_task_trials_rest.tStopRefresh = tThisFlipGlobal  # on global time
                        imagery_task_trials_rest.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.stopped')
                        # update status
                        imagery_task_trials_rest.status = FINISHED
                        imagery_task_trials_rest.setAutoDraw(False)
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, win=win)
                    return
                # pause experiment here if requested
                if thisExp.status == PAUSED:
                    pauseExperiment(
                        thisExp=thisExp, 
                        win=win, 
                        timers=[routineTimer], 
                        playbackComponents=[]
                    )
                    # skip the frame we paused on
                    continue
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    resting.forceEnded = routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in resting.components:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "resting" ---
            for thisComponent in resting.components:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            # store stop times for resting
            resting.tStop = globalClock.getTime(format='float')
            resting.tStopRefresh = tThisFlipGlobal
            thisExp.addData('resting.stopped', resting.tStop)
            # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
            if resting.maxDurationReached:
                routineTimer.addTime(-resting.maxDuration)
            elif resting.forceEnded:
                routineTimer.reset()
            else:
                routineTimer.addTime(-0.500000)
            thisExp.nextEntry()
            
        # completed 1.0 repeats of 'imagery_task_items'
        
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        
        # --- Prepare to start Routine "imagery_task_feedback" ---
        # create an object to store info about Routine imagery_task_feedback
        imagery_task_feedback = data.Routine(
            name='imagery_task_feedback',
            components=[imagery_task_feedback_title, imagery_task_feedback_text_1, imagery_task_feedback_response, imagery_task_feedback_proceed],
        )
        imagery_task_feedback.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from imagery_task_feedback_text
        ntrial = len(imagery_task_items.data['key_resp_vmi.keys'])  #numpy array with size=[ntrials,ntypes]
        nCorr = imagery_task_items.data['key_resp_vmi.corr'].sum() #.std(), .mean() also available
        meanRt = imagery_task_items.data['key_resp_vmi.rt'].mean()
        corrRate = nCorr/ntrial*100
        
        if corrRate > 75 :
            msg = "You answered %i percent of trials correctly with an average response time of %.2f second(s). Bravo!" %(corrRate,meanRt)
            colortext = 'green'
        else :
            msg = "You answered %i percent of trials correctly with an average response time of %.2f second(s). Did you have difficulty imagining the items for this category? Keep going!" %(corrRate,meanRt)
            colortext = 'red'
        # create starting attributes for imagery_task_feedback_response
        imagery_task_feedback_response.keys = []
        imagery_task_feedback_response.rt = []
        _imagery_task_feedback_response_allKeys = []
        # store start times for imagery_task_feedback
        imagery_task_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        imagery_task_feedback.tStart = globalClock.getTime(format='float')
        imagery_task_feedback.status = STARTED
        thisExp.addData('imagery_task_feedback.started', imagery_task_feedback.tStart)
        imagery_task_feedback.maxDuration = None
        # keep track of which components have finished
        imagery_task_feedbackComponents = imagery_task_feedback.components
        for thisComponent in imagery_task_feedback.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "imagery_task_feedback" ---
        # if trial has changed, end Routine now
        if isinstance(imagery_domain, data.TrialHandler2) and thisImagery_domain.thisN != imagery_domain.thisTrial.thisN:
            continueRoutine = False
        imagery_task_feedback.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *imagery_task_feedback_title* updates
            
            # if imagery_task_feedback_title is starting this frame...
            if imagery_task_feedback_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_feedback_title.frameNStart = frameN  # exact frame index
                imagery_task_feedback_title.tStart = t  # local t and not account for scr refresh
                imagery_task_feedback_title.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_feedback_title, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_feedback_title.status = STARTED
                imagery_task_feedback_title.setAutoDraw(True)
            
            # if imagery_task_feedback_title is active this frame...
            if imagery_task_feedback_title.status == STARTED:
                # update params
                pass
            
            # *imagery_task_feedback_text_1* updates
            
            # if imagery_task_feedback_text_1 is starting this frame...
            if imagery_task_feedback_text_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_feedback_text_1.frameNStart = frameN  # exact frame index
                imagery_task_feedback_text_1.tStart = t  # local t and not account for scr refresh
                imagery_task_feedback_text_1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_feedback_text_1, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_feedback_text_1.status = STARTED
                imagery_task_feedback_text_1.setAutoDraw(True)
            
            # if imagery_task_feedback_text_1 is active this frame...
            if imagery_task_feedback_text_1.status == STARTED:
                # update params
                imagery_task_feedback_text_1.setColor(colortext, colorSpace='rgb', log=False)
                imagery_task_feedback_text_1.setText(msg, log=False)
            
            # *imagery_task_feedback_response* updates
            waitOnFlip = False
            
            # if imagery_task_feedback_response is starting this frame...
            if imagery_task_feedback_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_feedback_response.frameNStart = frameN  # exact frame index
                imagery_task_feedback_response.tStart = t  # local t and not account for scr refresh
                imagery_task_feedback_response.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_feedback_response, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_feedback_response.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(imagery_task_feedback_response.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(imagery_task_feedback_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if imagery_task_feedback_response.status == STARTED and not waitOnFlip:
                theseKeys = imagery_task_feedback_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
                _imagery_task_feedback_response_allKeys.extend(theseKeys)
                if len(_imagery_task_feedback_response_allKeys):
                    imagery_task_feedback_response.keys = _imagery_task_feedback_response_allKeys[-1].name  # just the last key pressed
                    imagery_task_feedback_response.rt = _imagery_task_feedback_response_allKeys[-1].rt
                    imagery_task_feedback_response.duration = _imagery_task_feedback_response_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *imagery_task_feedback_proceed* updates
            
            # if imagery_task_feedback_proceed is starting this frame...
            if imagery_task_feedback_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_feedback_proceed.frameNStart = frameN  # exact frame index
                imagery_task_feedback_proceed.tStart = t  # local t and not account for scr refresh
                imagery_task_feedback_proceed.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_feedback_proceed, 'tStartRefresh')  # time at next scr refresh
                # update status
                imagery_task_feedback_proceed.status = STARTED
                imagery_task_feedback_proceed.setAutoDraw(True)
            
            # if imagery_task_feedback_proceed is active this frame...
            if imagery_task_feedback_proceed.status == STARTED:
                # update params
                pass
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                imagery_task_feedback.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in imagery_task_feedback.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "imagery_task_feedback" ---
        for thisComponent in imagery_task_feedback.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for imagery_task_feedback
        imagery_task_feedback.tStop = globalClock.getTime(format='float')
        imagery_task_feedback.tStopRefresh = tThisFlipGlobal
        thisExp.addData('imagery_task_feedback.stopped', imagery_task_feedback.tStop)
        # the Routine "imagery_task_feedback" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'imagery_domain'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # --- Prepare to start Routine "perception_instruction" ---
    # create an object to store info about Routine perception_instruction
    perception_instruction = data.Routine(
        name='perception_instruction',
        components=[perception_instruction_title, perception_instruction_task, perception_instruction_response, perception_instruction_proceed],
    )
    perception_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for perception_instruction_response
    perception_instruction_response.keys = []
    perception_instruction_response.rt = []
    _perception_instruction_response_allKeys = []
    # store start times for perception_instruction
    perception_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    perception_instruction.tStart = globalClock.getTime(format='float')
    perception_instruction.status = STARTED
    thisExp.addData('perception_instruction.started', perception_instruction.tStart)
    perception_instruction.maxDuration = None
    # keep track of which components have finished
    perception_instructionComponents = perception_instruction.components
    for thisComponent in perception_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "perception_instruction" ---
    perception_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *perception_instruction_title* updates
        
        # if perception_instruction_title is starting this frame...
        if perception_instruction_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_instruction_title.frameNStart = frameN  # exact frame index
            perception_instruction_title.tStart = t  # local t and not account for scr refresh
            perception_instruction_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_instruction_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_instruction_title.status = STARTED
            perception_instruction_title.setAutoDraw(True)
        
        # if perception_instruction_title is active this frame...
        if perception_instruction_title.status == STARTED:
            # update params
            pass
        
        # *perception_instruction_task* updates
        
        # if perception_instruction_task is starting this frame...
        if perception_instruction_task.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_instruction_task.frameNStart = frameN  # exact frame index
            perception_instruction_task.tStart = t  # local t and not account for scr refresh
            perception_instruction_task.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_instruction_task, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_instruction_task.status = STARTED
            perception_instruction_task.setAutoDraw(True)
        
        # if perception_instruction_task is active this frame...
        if perception_instruction_task.status == STARTED:
            # update params
            pass
        
        # *perception_instruction_response* updates
        waitOnFlip = False
        
        # if perception_instruction_response is starting this frame...
        if perception_instruction_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_instruction_response.frameNStart = frameN  # exact frame index
            perception_instruction_response.tStart = t  # local t and not account for scr refresh
            perception_instruction_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_instruction_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_instruction_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(perception_instruction_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(perception_instruction_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if perception_instruction_response.status == STARTED and not waitOnFlip:
            theseKeys = perception_instruction_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _perception_instruction_response_allKeys.extend(theseKeys)
            if len(_perception_instruction_response_allKeys):
                perception_instruction_response.keys = _perception_instruction_response_allKeys[-1].name  # just the last key pressed
                perception_instruction_response.rt = _perception_instruction_response_allKeys[-1].rt
                perception_instruction_response.duration = _perception_instruction_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *perception_instruction_proceed* updates
        
        # if perception_instruction_proceed is starting this frame...
        if perception_instruction_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_instruction_proceed.frameNStart = frameN  # exact frame index
            perception_instruction_proceed.tStart = t  # local t and not account for scr refresh
            perception_instruction_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_instruction_proceed, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'perception_instruction_proceed.started')
            # update status
            perception_instruction_proceed.status = STARTED
            perception_instruction_proceed.setAutoDraw(True)
        
        # if perception_instruction_proceed is active this frame...
        if perception_instruction_proceed.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            perception_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in perception_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "perception_instruction" ---
    for thisComponent in perception_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for perception_instruction
    perception_instruction.tStop = globalClock.getTime(format='float')
    perception_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('perception_instruction.stopped', perception_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "perception_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    perception_task_trial = data.TrialHandler2(
        name='perception_task_trial',
        nReps=1.0, 
        method='random', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions('PerceptionItems_en.xlsx'), 
        seed=None, 
    )
    thisExp.addLoop(perception_task_trial)  # add the loop to the experiment
    thisPerception_task_trial = perception_task_trial.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisPerception_task_trial.rgb)
    if thisPerception_task_trial != None:
        for paramName in thisPerception_task_trial:
            globals()[paramName] = thisPerception_task_trial[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisPerception_task_trial in perception_task_trial:
        currentLoop = perception_task_trial
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisPerception_task_trial.rgb)
        if thisPerception_task_trial != None:
            for paramName in thisPerception_task_trial:
                globals()[paramName] = thisPerception_task_trial[paramName]
        
        # --- Prepare to start Routine "perception_task" ---
        # create an object to store info about Routine perception_task
        perception_task = data.Routine(
            name='perception_task',
            components=[perception_task_title, perception_task_word1, perception_task_img1, perception_task_word2, perception_task_img2, perception_task_instruction, perception_task_img3, perception, perception_task_text],
        )
        perception_task.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        perception_task_word1.setSound(word1, secs=2, hamming=False)
        perception_task_word1.setVolume(8, log=False)
        perception_task_word1.seek(0)
        perception_task_img1.setImage(img1)
        perception_task_word2.setSound(word2, secs=2, hamming=False)
        perception_task_word2.setVolume(8, log=False)
        perception_task_word2.seek(0)
        perception_task_img2.setImage(img2)
        perception_task_instruction.setSound(instruction, hamming=False)
        perception_task_instruction.setVolume(8, log=False)
        perception_task_instruction.seek(0)
        perception_task_img3.setImage(imgCompare)
        # create starting attributes for perception
        perception.keys = []
        perception.rt = []
        _perception_allKeys = []
        perception_task_text.setText('Give your answer: first (key "1") or second (key "2").')
        # store start times for perception_task
        perception_task.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        perception_task.tStart = globalClock.getTime(format='float')
        perception_task.status = STARTED
        thisExp.addData('perception_task.started', perception_task.tStart)
        perception_task.maxDuration = None
        # keep track of which components have finished
        perception_taskComponents = perception_task.components
        for thisComponent in perception_task.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "perception_task" ---
        # if trial has changed, end Routine now
        if isinstance(perception_task_trial, data.TrialHandler2) and thisPerception_task_trial.thisN != perception_task_trial.thisTrial.thisN:
            continueRoutine = False
        perception_task.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *perception_task_title* updates
            
            # if perception_task_title is starting this frame...
            if perception_task_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_title.frameNStart = frameN  # exact frame index
                perception_task_title.tStart = t  # local t and not account for scr refresh
                perception_task_title.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_title, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_title.status = STARTED
                perception_task_title.setAutoDraw(True)
            
            # if perception_task_title is active this frame...
            if perception_task_title.status == STARTED:
                # update params
                pass
            
            # *perception_task_word1* updates
            
            # if perception_task_word1 is starting this frame...
            if perception_task_word1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_word1.frameNStart = frameN  # exact frame index
                perception_task_word1.tStart = t  # local t and not account for scr refresh
                perception_task_word1.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                perception_task_word1.status = STARTED
                perception_task_word1.play(when=win)  # sync with win flip
            
            # if perception_task_word1 is stopping this frame...
            if perception_task_word1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > perception_task_word1.tStartRefresh + 2-frameTolerance or perception_task_word1.isFinished:
                    # keep track of stop time/frame for later
                    perception_task_word1.tStop = t  # not accounting for scr refresh
                    perception_task_word1.tStopRefresh = tThisFlipGlobal  # on global time
                    perception_task_word1.frameNStop = frameN  # exact frame index
                    # update status
                    perception_task_word1.status = FINISHED
                    perception_task_word1.stop()
            
            # *perception_task_img1* updates
            
            # if perception_task_img1 is starting this frame...
            if perception_task_img1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_img1.frameNStart = frameN  # exact frame index
                perception_task_img1.tStart = t  # local t and not account for scr refresh
                perception_task_img1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_img1, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_img1.status = STARTED
                perception_task_img1.setAutoDraw(True)
            
            # if perception_task_img1 is active this frame...
            if perception_task_img1.status == STARTED:
                # update params
                pass
            
            # if perception_task_img1 is stopping this frame...
            if perception_task_img1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > perception_task_img1.tStartRefresh + 2-frameTolerance:
                    # keep track of stop time/frame for later
                    perception_task_img1.tStop = t  # not accounting for scr refresh
                    perception_task_img1.tStopRefresh = tThisFlipGlobal  # on global time
                    perception_task_img1.frameNStop = frameN  # exact frame index
                    # update status
                    perception_task_img1.status = FINISHED
                    perception_task_img1.setAutoDraw(False)
            
            # *perception_task_word2* updates
            
            # if perception_task_word2 is starting this frame...
            if perception_task_word2.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                # keep track of start time/frame for later
                perception_task_word2.frameNStart = frameN  # exact frame index
                perception_task_word2.tStart = t  # local t and not account for scr refresh
                perception_task_word2.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                perception_task_word2.status = STARTED
                perception_task_word2.play(when=win)  # sync with win flip
            
            # if perception_task_word2 is stopping this frame...
            if perception_task_word2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > perception_task_word2.tStartRefresh + 2-frameTolerance or perception_task_word2.isFinished:
                    # keep track of stop time/frame for later
                    perception_task_word2.tStop = t  # not accounting for scr refresh
                    perception_task_word2.tStopRefresh = tThisFlipGlobal  # on global time
                    perception_task_word2.frameNStop = frameN  # exact frame index
                    # update status
                    perception_task_word2.status = FINISHED
                    perception_task_word2.stop()
            
            # *perception_task_img2* updates
            
            # if perception_task_img2 is starting this frame...
            if perception_task_img2.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                # keep track of start time/frame for later
                perception_task_img2.frameNStart = frameN  # exact frame index
                perception_task_img2.tStart = t  # local t and not account for scr refresh
                perception_task_img2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_img2, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_img2.status = STARTED
                perception_task_img2.setAutoDraw(True)
            
            # if perception_task_img2 is active this frame...
            if perception_task_img2.status == STARTED:
                # update params
                pass
            
            # if perception_task_img2 is stopping this frame...
            if perception_task_img2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > perception_task_img2.tStartRefresh + 2-frameTolerance:
                    # keep track of stop time/frame for later
                    perception_task_img2.tStop = t  # not accounting for scr refresh
                    perception_task_img2.tStopRefresh = tThisFlipGlobal  # on global time
                    perception_task_img2.frameNStop = frameN  # exact frame index
                    # update status
                    perception_task_img2.status = FINISHED
                    perception_task_img2.setAutoDraw(False)
            
            # *perception_task_instruction* updates
            
            # if perception_task_instruction is starting this frame...
            if perception_task_instruction.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                # keep track of start time/frame for later
                perception_task_instruction.frameNStart = frameN  # exact frame index
                perception_task_instruction.tStart = t  # local t and not account for scr refresh
                perception_task_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                perception_task_instruction.status = STARTED
                perception_task_instruction.play(when=win)  # sync with win flip
            
            # if perception_task_instruction is stopping this frame...
            if perception_task_instruction.status == STARTED:
                if bool(False) or perception_task_instruction.isFinished:
                    # keep track of stop time/frame for later
                    perception_task_instruction.tStop = t  # not accounting for scr refresh
                    perception_task_instruction.tStopRefresh = tThisFlipGlobal  # on global time
                    perception_task_instruction.frameNStop = frameN  # exact frame index
                    # update status
                    perception_task_instruction.status = FINISHED
                    perception_task_instruction.stop()
            
            # *perception_task_img3* updates
            
            # if perception_task_img3 is starting this frame...
            if perception_task_img3.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                # keep track of start time/frame for later
                perception_task_img3.frameNStart = frameN  # exact frame index
                perception_task_img3.tStart = t  # local t and not account for scr refresh
                perception_task_img3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_img3, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_img3.status = STARTED
                perception_task_img3.setAutoDraw(True)
            
            # if perception_task_img3 is active this frame...
            if perception_task_img3.status == STARTED:
                # update params
                pass
            
            # *perception* updates
            waitOnFlip = False
            
            # if perception is starting this frame...
            if perception.status == NOT_STARTED and perception_task_instruction.status==FINISHED:
                # keep track of start time/frame for later
                perception.frameNStart = frameN  # exact frame index
                perception.tStart = t  # local t and not account for scr refresh
                perception.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'perception.started')
                # update status
                perception.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(perception.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(perception.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if perception.status == STARTED and not waitOnFlip:
                theseKeys = perception.getKeys(keyList=['1','2'], ignoreKeys=None, waitRelease=False)
                _perception_allKeys.extend(theseKeys)
                if len(_perception_allKeys):
                    perception.keys = _perception_allKeys[-1].name  # just the last key pressed
                    perception.rt = _perception_allKeys[-1].rt
                    perception.duration = _perception_allKeys[-1].duration
                    # was this correct?
                    if (perception.keys == str(correctAns)) or (perception.keys == correctAns):
                        perception.corr = 1
                    else:
                        perception.corr = 0
                    # a response ends the routine
                    continueRoutine = False
            
            # *perception_task_text* updates
            
            # if perception_task_text is starting this frame...
            if perception_task_text.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_text.frameNStart = frameN  # exact frame index
                perception_task_text.tStart = t  # local t and not account for scr refresh
                perception_task_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_text, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_text.status = STARTED
                perception_task_text.setAutoDraw(True)
            
            # if perception_task_text is active this frame...
            if perception_task_text.status == STARTED:
                # update params
                pass
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[perception_task_word1, perception_task_word2, perception_task_instruction]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                perception_task.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in perception_task.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "perception_task" ---
        for thisComponent in perception_task.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for perception_task
        perception_task.tStop = globalClock.getTime(format='float')
        perception_task.tStopRefresh = tThisFlipGlobal
        thisExp.addData('perception_task.stopped', perception_task.tStop)
        perception_task_word1.pause()  # ensure sound has stopped at end of Routine
        perception_task_word2.pause()  # ensure sound has stopped at end of Routine
        perception_task_instruction.pause()  # ensure sound has stopped at end of Routine
        # check responses
        if perception.keys in ['', [], None]:  # No response was made
            perception.keys = None
            # was no response the correct answer?!
            if str(correctAns).lower() == 'none':
               perception.corr = 1;  # correct non-response
            else:
               perception.corr = 0;  # failed to respond (incorrectly)
        # store data for perception_task_trial (TrialHandler)
        perception_task_trial.addData('perception.keys',perception.keys)
        perception_task_trial.addData('perception.corr', perception.corr)
        if perception.keys != None:  # we had a response
            perception_task_trial.addData('perception.rt', perception.rt)
            perception_task_trial.addData('perception.duration', perception.duration)
        # the Routine "perception_task" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "perception_task_confidence" ---
        # create an object to store info about Routine perception_task_confidence
        perception_task_confidence = data.Routine(
            name='perception_task_confidence',
            components=[perception_task_confidence_idx, perception_task_confidence_text, confidence],
        )
        perception_task_confidence.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        perception_task_confidence_idx.setText(perception_task_trial.thisN)
        # create starting attributes for confidence
        confidence.keys = []
        confidence.rt = []
        _confidence_allKeys = []
        # store start times for perception_task_confidence
        perception_task_confidence.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        perception_task_confidence.tStart = globalClock.getTime(format='float')
        perception_task_confidence.status = STARTED
        thisExp.addData('perception_task_confidence.started', perception_task_confidence.tStart)
        perception_task_confidence.maxDuration = None
        # keep track of which components have finished
        perception_task_confidenceComponents = perception_task_confidence.components
        for thisComponent in perception_task_confidence.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "perception_task_confidence" ---
        # if trial has changed, end Routine now
        if isinstance(perception_task_trial, data.TrialHandler2) and thisPerception_task_trial.thisN != perception_task_trial.thisTrial.thisN:
            continueRoutine = False
        perception_task_confidence.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *perception_task_confidence_idx* updates
            
            # if perception_task_confidence_idx is starting this frame...
            if perception_task_confidence_idx.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_confidence_idx.frameNStart = frameN  # exact frame index
                perception_task_confidence_idx.tStart = t  # local t and not account for scr refresh
                perception_task_confidence_idx.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_confidence_idx, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_confidence_idx.status = STARTED
                perception_task_confidence_idx.setAutoDraw(True)
            
            # if perception_task_confidence_idx is active this frame...
            if perception_task_confidence_idx.status == STARTED:
                # update params
                pass
            
            # *perception_task_confidence_text* updates
            
            # if perception_task_confidence_text is starting this frame...
            if perception_task_confidence_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                perception_task_confidence_text.frameNStart = frameN  # exact frame index
                perception_task_confidence_text.tStart = t  # local t and not account for scr refresh
                perception_task_confidence_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(perception_task_confidence_text, 'tStartRefresh')  # time at next scr refresh
                # update status
                perception_task_confidence_text.status = STARTED
                perception_task_confidence_text.setAutoDraw(True)
            
            # if perception_task_confidence_text is active this frame...
            if perception_task_confidence_text.status == STARTED:
                # update params
                pass
            
            # *confidence* updates
            waitOnFlip = False
            
            # if confidence is starting this frame...
            if confidence.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                confidence.frameNStart = frameN  # exact frame index
                confidence.tStart = t  # local t and not account for scr refresh
                confidence.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(confidence, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'confidence.started')
                # update status
                confidence.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(confidence.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(confidence.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if confidence.status == STARTED and not waitOnFlip:
                theseKeys = confidence.getKeys(keyList=['1','2','3','4'], ignoreKeys=None, waitRelease=False)
                _confidence_allKeys.extend(theseKeys)
                if len(_confidence_allKeys):
                    confidence.keys = _confidence_allKeys[-1].name  # just the last key pressed
                    confidence.rt = _confidence_allKeys[-1].rt
                    confidence.duration = _confidence_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                perception_task_confidence.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in perception_task_confidence.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "perception_task_confidence" ---
        for thisComponent in perception_task_confidence.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for perception_task_confidence
        perception_task_confidence.tStop = globalClock.getTime(format='float')
        perception_task_confidence.tStopRefresh = tThisFlipGlobal
        thisExp.addData('perception_task_confidence.stopped', perception_task_confidence.tStop)
        # check responses
        if confidence.keys in ['', [], None]:  # No response was made
            confidence.keys = None
        perception_task_trial.addData('confidence.keys',confidence.keys)
        if confidence.keys != None:  # we had a response
            perception_task_trial.addData('confidence.rt', confidence.rt)
            perception_task_trial.addData('confidence.duration', confidence.duration)
        # the Routine "perception_task_confidence" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "resting" ---
        # create an object to store info about Routine resting
        resting = data.Routine(
            name='resting',
            components=[imagery_task_trials_rest],
        )
        resting.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        # store start times for resting
        resting.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        resting.tStart = globalClock.getTime(format='float')
        resting.status = STARTED
        thisExp.addData('resting.started', resting.tStart)
        resting.maxDuration = None
        # keep track of which components have finished
        restingComponents = resting.components
        for thisComponent in resting.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "resting" ---
        # if trial has changed, end Routine now
        if isinstance(perception_task_trial, data.TrialHandler2) and thisPerception_task_trial.thisN != perception_task_trial.thisTrial.thisN:
            continueRoutine = False
        resting.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine and routineTimer.getTime() < 0.5:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *imagery_task_trials_rest* updates
            
            # if imagery_task_trials_rest is starting this frame...
            if imagery_task_trials_rest.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                imagery_task_trials_rest.frameNStart = frameN  # exact frame index
                imagery_task_trials_rest.tStart = t  # local t and not account for scr refresh
                imagery_task_trials_rest.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(imagery_task_trials_rest, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.started')
                # update status
                imagery_task_trials_rest.status = STARTED
                imagery_task_trials_rest.setAutoDraw(True)
            
            # if imagery_task_trials_rest is active this frame...
            if imagery_task_trials_rest.status == STARTED:
                # update params
                pass
            
            # if imagery_task_trials_rest is stopping this frame...
            if imagery_task_trials_rest.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > imagery_task_trials_rest.tStartRefresh + 0.5-frameTolerance:
                    # keep track of stop time/frame for later
                    imagery_task_trials_rest.tStop = t  # not accounting for scr refresh
                    imagery_task_trials_rest.tStopRefresh = tThisFlipGlobal  # on global time
                    imagery_task_trials_rest.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'imagery_task_trials_rest.stopped')
                    # update status
                    imagery_task_trials_rest.status = FINISHED
                    imagery_task_trials_rest.setAutoDraw(False)
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                resting.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in resting.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "resting" ---
        for thisComponent in resting.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for resting
        resting.tStop = globalClock.getTime(format='float')
        resting.tStopRefresh = tThisFlipGlobal
        thisExp.addData('resting.stopped', resting.tStop)
        # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
        if resting.maxDurationReached:
            routineTimer.addTime(-resting.maxDuration)
        elif resting.forceEnded:
            routineTimer.reset()
        else:
            routineTimer.addTime(-0.500000)
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'perception_task_trial'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # --- Prepare to start Routine "perception_task_feedback" ---
    # create an object to store info about Routine perception_task_feedback
    perception_task_feedback = data.Routine(
        name='perception_task_feedback',
        components=[perception_task_feedback_title, perception_task_feedback_text_1, perception_task_feedback_response, perception_task_feedback_proceed],
    )
    perception_task_feedback.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from perception_task_feedback_text
    ntrial = len(perception_task_trial.data['perception.keys'])  #numpy array with size=[ntrials,ntypes]
    nCorr = perception_task_trial.data['perception.corr'].sum() #.std(), .mean() also available
    meanRt = perception_task_trial.data['perception.rt'].mean()
    corrRate = nCorr/ntrial*100
    
    if corrRate > 75 :
        msg = "You answered %i percent of trials correctly with an average response time of %.2f second(s). Bravo!" %(corrRate,meanRt)
        colortext = 'green'
    else :
        msg = "You answered %i percent of trials correctly with an average response time of %.2f second(s). Did you have difficulty imagining the items for this category? Keep going!" %(corrRate,meanRt)
        colortext = 'red'
    # create starting attributes for perception_task_feedback_response
    perception_task_feedback_response.keys = []
    perception_task_feedback_response.rt = []
    _perception_task_feedback_response_allKeys = []
    # store start times for perception_task_feedback
    perception_task_feedback.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    perception_task_feedback.tStart = globalClock.getTime(format='float')
    perception_task_feedback.status = STARTED
    thisExp.addData('perception_task_feedback.started', perception_task_feedback.tStart)
    perception_task_feedback.maxDuration = None
    # keep track of which components have finished
    perception_task_feedbackComponents = perception_task_feedback.components
    for thisComponent in perception_task_feedback.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "perception_task_feedback" ---
    perception_task_feedback.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *perception_task_feedback_title* updates
        
        # if perception_task_feedback_title is starting this frame...
        if perception_task_feedback_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_task_feedback_title.frameNStart = frameN  # exact frame index
            perception_task_feedback_title.tStart = t  # local t and not account for scr refresh
            perception_task_feedback_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_task_feedback_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_task_feedback_title.status = STARTED
            perception_task_feedback_title.setAutoDraw(True)
        
        # if perception_task_feedback_title is active this frame...
        if perception_task_feedback_title.status == STARTED:
            # update params
            pass
        
        # *perception_task_feedback_text_1* updates
        
        # if perception_task_feedback_text_1 is starting this frame...
        if perception_task_feedback_text_1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_task_feedback_text_1.frameNStart = frameN  # exact frame index
            perception_task_feedback_text_1.tStart = t  # local t and not account for scr refresh
            perception_task_feedback_text_1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_task_feedback_text_1, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_task_feedback_text_1.status = STARTED
            perception_task_feedback_text_1.setAutoDraw(True)
        
        # if perception_task_feedback_text_1 is active this frame...
        if perception_task_feedback_text_1.status == STARTED:
            # update params
            perception_task_feedback_text_1.setColor(colortext, colorSpace='rgb', log=False)
            perception_task_feedback_text_1.setText(msg, log=False)
        
        # *perception_task_feedback_response* updates
        waitOnFlip = False
        
        # if perception_task_feedback_response is starting this frame...
        if perception_task_feedback_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_task_feedback_response.frameNStart = frameN  # exact frame index
            perception_task_feedback_response.tStart = t  # local t and not account for scr refresh
            perception_task_feedback_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_task_feedback_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_task_feedback_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(perception_task_feedback_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(perception_task_feedback_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if perception_task_feedback_response.status == STARTED and not waitOnFlip:
            theseKeys = perception_task_feedback_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _perception_task_feedback_response_allKeys.extend(theseKeys)
            if len(_perception_task_feedback_response_allKeys):
                perception_task_feedback_response.keys = _perception_task_feedback_response_allKeys[-1].name  # just the last key pressed
                perception_task_feedback_response.rt = _perception_task_feedback_response_allKeys[-1].rt
                perception_task_feedback_response.duration = _perception_task_feedback_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *perception_task_feedback_proceed* updates
        
        # if perception_task_feedback_proceed is starting this frame...
        if perception_task_feedback_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            perception_task_feedback_proceed.frameNStart = frameN  # exact frame index
            perception_task_feedback_proceed.tStart = t  # local t and not account for scr refresh
            perception_task_feedback_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(perception_task_feedback_proceed, 'tStartRefresh')  # time at next scr refresh
            # update status
            perception_task_feedback_proceed.status = STARTED
            perception_task_feedback_proceed.setAutoDraw(True)
        
        # if perception_task_feedback_proceed is active this frame...
        if perception_task_feedback_proceed.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            perception_task_feedback.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in perception_task_feedback.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "perception_task_feedback" ---
    for thisComponent in perception_task_feedback.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for perception_task_feedback
    perception_task_feedback.tStop = globalClock.getTime(format='float')
    perception_task_feedback.tStopRefresh = tThisFlipGlobal
    thisExp.addData('perception_task_feedback.stopped', perception_task_feedback.tStop)
    thisExp.nextEntry()
    # the Routine "perception_task_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "sem_instruction" ---
    # create an object to store info about Routine sem_instruction
    sem_instruction = data.Routine(
        name='sem_instruction',
        components=[semantics_instruction_title, semantics_instruction_task, semantics_instruction_response, semantics_instruction_proceed],
    )
    sem_instruction.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # create starting attributes for semantics_instruction_response
    semantics_instruction_response.keys = []
    semantics_instruction_response.rt = []
    _semantics_instruction_response_allKeys = []
    # store start times for sem_instruction
    sem_instruction.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    sem_instruction.tStart = globalClock.getTime(format='float')
    sem_instruction.status = STARTED
    thisExp.addData('sem_instruction.started', sem_instruction.tStart)
    sem_instruction.maxDuration = None
    # keep track of which components have finished
    sem_instructionComponents = sem_instruction.components
    for thisComponent in sem_instruction.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "sem_instruction" ---
    sem_instruction.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *semantics_instruction_title* updates
        
        # if semantics_instruction_title is starting this frame...
        if semantics_instruction_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            semantics_instruction_title.frameNStart = frameN  # exact frame index
            semantics_instruction_title.tStart = t  # local t and not account for scr refresh
            semantics_instruction_title.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(semantics_instruction_title, 'tStartRefresh')  # time at next scr refresh
            # update status
            semantics_instruction_title.status = STARTED
            semantics_instruction_title.setAutoDraw(True)
        
        # if semantics_instruction_title is active this frame...
        if semantics_instruction_title.status == STARTED:
            # update params
            pass
        
        # *semantics_instruction_task* updates
        
        # if semantics_instruction_task is starting this frame...
        if semantics_instruction_task.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            semantics_instruction_task.frameNStart = frameN  # exact frame index
            semantics_instruction_task.tStart = t  # local t and not account for scr refresh
            semantics_instruction_task.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(semantics_instruction_task, 'tStartRefresh')  # time at next scr refresh
            # update status
            semantics_instruction_task.status = STARTED
            semantics_instruction_task.setAutoDraw(True)
        
        # if semantics_instruction_task is active this frame...
        if semantics_instruction_task.status == STARTED:
            # update params
            pass
        
        # *semantics_instruction_response* updates
        waitOnFlip = False
        
        # if semantics_instruction_response is starting this frame...
        if semantics_instruction_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            semantics_instruction_response.frameNStart = frameN  # exact frame index
            semantics_instruction_response.tStart = t  # local t and not account for scr refresh
            semantics_instruction_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(semantics_instruction_response, 'tStartRefresh')  # time at next scr refresh
            # update status
            semantics_instruction_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(semantics_instruction_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(semantics_instruction_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if semantics_instruction_response.status == STARTED and not waitOnFlip:
            theseKeys = semantics_instruction_response.getKeys(keyList=['space'], ignoreKeys=None, waitRelease=False)
            _semantics_instruction_response_allKeys.extend(theseKeys)
            if len(_semantics_instruction_response_allKeys):
                semantics_instruction_response.keys = _semantics_instruction_response_allKeys[-1].name  # just the last key pressed
                semantics_instruction_response.rt = _semantics_instruction_response_allKeys[-1].rt
                semantics_instruction_response.duration = _semantics_instruction_response_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # *semantics_instruction_proceed* updates
        
        # if semantics_instruction_proceed is starting this frame...
        if semantics_instruction_proceed.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            semantics_instruction_proceed.frameNStart = frameN  # exact frame index
            semantics_instruction_proceed.tStart = t  # local t and not account for scr refresh
            semantics_instruction_proceed.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(semantics_instruction_proceed, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'semantics_instruction_proceed.started')
            # update status
            semantics_instruction_proceed.status = STARTED
            semantics_instruction_proceed.setAutoDraw(True)
        
        # if semantics_instruction_proceed is active this frame...
        if semantics_instruction_proceed.status == STARTED:
            # update params
            pass
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            sem_instruction.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in sem_instruction.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "sem_instruction" ---
    for thisComponent in sem_instruction.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for sem_instruction
    sem_instruction.tStop = globalClock.getTime(format='float')
    sem_instruction.tStopRefresh = tThisFlipGlobal
    thisExp.addData('sem_instruction.stopped', sem_instruction.tStop)
    thisExp.nextEntry()
    # the Routine "sem_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    trials = data.TrialHandler2(
        name='trials',
        nReps=1.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions('SemanticsPractice_en.xlsx'), 
        seed=None, 
    )
    thisExp.addLoop(trials)  # add the loop to the experiment
    thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            globals()[paramName] = thisTrial[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisTrial in trials:
        currentLoop = trials
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
        if thisTrial != None:
            for paramName in thisTrial:
                globals()[paramName] = thisTrial[paramName]
        
        # --- Prepare to start Routine "semantics_task_trials" ---
        # create an object to store info about Routine semantics_task_trials
        semantics_task_trials = data.Routine(
            name='semantics_task_trials',
            components=[semantics_task_trials_question, semantics_task_trials_word1, semantics_task_trials_word2, semantics_task_trials_instruction, semantics_task_trials_response, semantics_task_trials_order_instruction, semantics_task_trials_proceed],
        )
        semantics_task_trials.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        semantics_task_trials_question.setText(task_question)
        semantics_task_trials_word1.setSound(word1, secs=2, hamming=False)
        semantics_task_trials_word1.setVolume(8, log=False)
        semantics_task_trials_word1.seek(0)
        semantics_task_trials_word2.setSound(word2, secs=2, hamming=False)
        semantics_task_trials_word2.setVolume(8, log=False)
        semantics_task_trials_word2.seek(0)
        semantics_task_trials_instruction.setSound(instruction, hamming=False)
        semantics_task_trials_instruction.setVolume(8, log=False)
        semantics_task_trials_instruction.seek(0)
        # create starting attributes for semantics_task_trials_response
        semantics_task_trials_response.keys = []
        semantics_task_trials_response.rt = []
        _semantics_task_trials_response_allKeys = []
        semantics_task_trials_order_instruction.setText(order_instruction)
        semantics_task_trials_proceed.setText(task_continue)
        # store start times for semantics_task_trials
        semantics_task_trials.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        semantics_task_trials.tStart = globalClock.getTime(format='float')
        semantics_task_trials.status = STARTED
        thisExp.addData('semantics_task_trials.started', semantics_task_trials.tStart)
        semantics_task_trials.maxDuration = None
        # keep track of which components have finished
        semantics_task_trialsComponents = semantics_task_trials.components
        for thisComponent in semantics_task_trials.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "semantics_task_trials" ---
        # if trial has changed, end Routine now
        if isinstance(trials, data.TrialHandler2) and thisTrial.thisN != trials.thisTrial.thisN:
            continueRoutine = False
        semantics_task_trials.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *semantics_task_trials_question* updates
            
            # if semantics_task_trials_question is starting this frame...
            if semantics_task_trials_question.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_question.frameNStart = frameN  # exact frame index
                semantics_task_trials_question.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_question.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semantics_task_trials_question, 'tStartRefresh')  # time at next scr refresh
                # update status
                semantics_task_trials_question.status = STARTED
                semantics_task_trials_question.setAutoDraw(True)
            
            # if semantics_task_trials_question is active this frame...
            if semantics_task_trials_question.status == STARTED:
                # update params
                pass
            
            # *semantics_task_trials_word1* updates
            
            # if semantics_task_trials_word1 is starting this frame...
            if semantics_task_trials_word1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_word1.frameNStart = frameN  # exact frame index
                semantics_task_trials_word1.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_word1.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                semantics_task_trials_word1.status = STARTED
                semantics_task_trials_word1.play(when=win)  # sync with win flip
            
            # if semantics_task_trials_word1 is stopping this frame...
            if semantics_task_trials_word1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > semantics_task_trials_word1.tStartRefresh + 2-frameTolerance or semantics_task_trials_word1.isFinished:
                    # keep track of stop time/frame for later
                    semantics_task_trials_word1.tStop = t  # not accounting for scr refresh
                    semantics_task_trials_word1.tStopRefresh = tThisFlipGlobal  # on global time
                    semantics_task_trials_word1.frameNStop = frameN  # exact frame index
                    # update status
                    semantics_task_trials_word1.status = FINISHED
                    semantics_task_trials_word1.stop()
            
            # *semantics_task_trials_word2* updates
            
            # if semantics_task_trials_word2 is starting this frame...
            if semantics_task_trials_word2.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_word2.frameNStart = frameN  # exact frame index
                semantics_task_trials_word2.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_word2.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                semantics_task_trials_word2.status = STARTED
                semantics_task_trials_word2.play(when=win)  # sync with win flip
            
            # if semantics_task_trials_word2 is stopping this frame...
            if semantics_task_trials_word2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > semantics_task_trials_word2.tStartRefresh + 2-frameTolerance or semantics_task_trials_word2.isFinished:
                    # keep track of stop time/frame for later
                    semantics_task_trials_word2.tStop = t  # not accounting for scr refresh
                    semantics_task_trials_word2.tStopRefresh = tThisFlipGlobal  # on global time
                    semantics_task_trials_word2.frameNStop = frameN  # exact frame index
                    # update status
                    semantics_task_trials_word2.status = FINISHED
                    semantics_task_trials_word2.stop()
            
            # *semantics_task_trials_instruction* updates
            
            # if semantics_task_trials_instruction is starting this frame...
            if semantics_task_trials_instruction.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_instruction.frameNStart = frameN  # exact frame index
                semantics_task_trials_instruction.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                # update status
                semantics_task_trials_instruction.status = STARTED
                semantics_task_trials_instruction.play(when=win)  # sync with win flip
            
            # if semantics_task_trials_instruction is stopping this frame...
            if semantics_task_trials_instruction.status == STARTED:
                if bool(False) or semantics_task_trials_instruction.isFinished:
                    # keep track of stop time/frame for later
                    semantics_task_trials_instruction.tStop = t  # not accounting for scr refresh
                    semantics_task_trials_instruction.tStopRefresh = tThisFlipGlobal  # on global time
                    semantics_task_trials_instruction.frameNStop = frameN  # exact frame index
                    # update status
                    semantics_task_trials_instruction.status = FINISHED
                    semantics_task_trials_instruction.stop()
            
            # *semantics_task_trials_response* updates
            waitOnFlip = False
            
            # if semantics_task_trials_response is starting this frame...
            if semantics_task_trials_response.status == NOT_STARTED and semantics_task_trials_instruction.status==FINISHED:
                # keep track of start time/frame for later
                semantics_task_trials_response.frameNStart = frameN  # exact frame index
                semantics_task_trials_response.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_response.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semantics_task_trials_response, 'tStartRefresh')  # time at next scr refresh
                # update status
                semantics_task_trials_response.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(semantics_task_trials_response.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(semantics_task_trials_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if semantics_task_trials_response.status == STARTED and not waitOnFlip:
                theseKeys = semantics_task_trials_response.getKeys(keyList=['1','2'], ignoreKeys=None, waitRelease=False)
                _semantics_task_trials_response_allKeys.extend(theseKeys)
                if len(_semantics_task_trials_response_allKeys):
                    semantics_task_trials_response.keys = _semantics_task_trials_response_allKeys[-1].name  # just the last key pressed
                    semantics_task_trials_response.rt = _semantics_task_trials_response_allKeys[-1].rt
                    semantics_task_trials_response.duration = _semantics_task_trials_response_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # *semantics_task_trials_order_instruction* updates
            
            # if semantics_task_trials_order_instruction is starting this frame...
            if semantics_task_trials_order_instruction.status == NOT_STARTED and tThisFlip >= 4.5-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_order_instruction.frameNStart = frameN  # exact frame index
                semantics_task_trials_order_instruction.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_order_instruction.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semantics_task_trials_order_instruction, 'tStartRefresh')  # time at next scr refresh
                # update status
                semantics_task_trials_order_instruction.status = STARTED
                semantics_task_trials_order_instruction.setAutoDraw(True)
            
            # if semantics_task_trials_order_instruction is active this frame...
            if semantics_task_trials_order_instruction.status == STARTED:
                # update params
                pass
            
            # *semantics_task_trials_proceed* updates
            
            # if semantics_task_trials_proceed is starting this frame...
            if semantics_task_trials_proceed.status == NOT_STARTED and tThisFlip >= 4.5-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_trials_proceed.frameNStart = frameN  # exact frame index
                semantics_task_trials_proceed.tStart = t  # local t and not account for scr refresh
                semantics_task_trials_proceed.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semantics_task_trials_proceed, 'tStartRefresh')  # time at next scr refresh
                # update status
                semantics_task_trials_proceed.status = STARTED
                semantics_task_trials_proceed.setAutoDraw(True)
            
            # if semantics_task_trials_proceed is active this frame...
            if semantics_task_trials_proceed.status == STARTED:
                # update params
                pass
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[semantics_task_trials_word1, semantics_task_trials_word2, semantics_task_trials_instruction]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                semantics_task_trials.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in semantics_task_trials.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "semantics_task_trials" ---
        for thisComponent in semantics_task_trials.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for semantics_task_trials
        semantics_task_trials.tStop = globalClock.getTime(format='float')
        semantics_task_trials.tStopRefresh = tThisFlipGlobal
        thisExp.addData('semantics_task_trials.stopped', semantics_task_trials.tStop)
        semantics_task_trials_word1.pause()  # ensure sound has stopped at end of Routine
        semantics_task_trials_word2.pause()  # ensure sound has stopped at end of Routine
        semantics_task_trials_instruction.pause()  # ensure sound has stopped at end of Routine
        # the Routine "semantics_task_trials" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'trials'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # set up handler to look after randomisation of conditions etc
    semanticBlock = data.TrialHandler2(
        name='semanticBlock',
        nReps=1.0, 
        method='sequential', 
        extraInfo=expInfo, 
        originPath=-1, 
        trialList=data.importConditions('SemanticItems.xlsx'), 
        seed=None, 
    )
    thisExp.addLoop(semanticBlock)  # add the loop to the experiment
    thisSemanticBlock = semanticBlock.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisSemanticBlock.rgb)
    if thisSemanticBlock != None:
        for paramName in thisSemanticBlock:
            globals()[paramName] = thisSemanticBlock[paramName]
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    for thisSemanticBlock in semanticBlock:
        currentLoop = semanticBlock
        thisExp.timestampOnFlip(win, 'thisRow.t', format=globalClock.format)
        if thisSession is not None:
            # if running in a Session with a Liaison client, send data up to now
            thisSession.sendExperimentData()
        # abbreviate parameter names if possible (e.g. rgb = thisSemanticBlock.rgb)
        if thisSemanticBlock != None:
            for paramName in thisSemanticBlock:
                globals()[paramName] = thisSemanticBlock[paramName]
        
        # --- Prepare to start Routine "sem_trial" ---
        # create an object to store info about Routine sem_trial
        sem_trial = data.Routine(
            name='sem_trial',
            components=[semantics_task_title, semanticBlock_idx, fixation, field_s, word1_s, word2_s, instr_s, key_resp_vmi_s],
        )
        sem_trial.status = NOT_STARTED
        continueRoutine = True
        # update component parameters for each repeat
        semanticBlock_idx.setText(semanticBlock.thisN)
        field_s.setSound(field, secs=2, hamming=True)
        field_s.setVolume(1.0, log=False)
        field_s.seek(0)
        word1_s.setSound(word1_sem, secs=2, hamming=True)
        word1_s.setVolume(1.0, log=False)
        word1_s.seek(0)
        word2_s.setSound(word2_sem, secs=2, hamming=True)
        word2_s.setVolume(1.0, log=False)
        word2_s.seek(0)
        instr_s.setSound(instruction_sem, hamming=True)
        instr_s.setVolume(1.0, log=False)
        instr_s.seek(0)
        # create starting attributes for key_resp_vmi_s
        key_resp_vmi_s.keys = []
        key_resp_vmi_s.rt = []
        _key_resp_vmi_s_allKeys = []
        # store start times for sem_trial
        sem_trial.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
        sem_trial.tStart = globalClock.getTime(format='float')
        sem_trial.status = STARTED
        thisExp.addData('sem_trial.started', sem_trial.tStart)
        sem_trial.maxDuration = None
        # keep track of which components have finished
        sem_trialComponents = sem_trial.components
        for thisComponent in sem_trial.components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "sem_trial" ---
        # if trial has changed, end Routine now
        if isinstance(semanticBlock, data.TrialHandler2) and thisSemanticBlock.thisN != semanticBlock.thisTrial.thisN:
            continueRoutine = False
        sem_trial.forceEnded = routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *semantics_task_title* updates
            
            # if semantics_task_title is starting this frame...
            if semantics_task_title.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                semantics_task_title.frameNStart = frameN  # exact frame index
                semantics_task_title.tStart = t  # local t and not account for scr refresh
                semantics_task_title.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semantics_task_title, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'semantics_task_title.started')
                # update status
                semantics_task_title.status = STARTED
                semantics_task_title.setAutoDraw(True)
            
            # if semantics_task_title is active this frame...
            if semantics_task_title.status == STARTED:
                # update params
                pass
            
            # *semanticBlock_idx* updates
            
            # if semanticBlock_idx is starting this frame...
            if semanticBlock_idx.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                semanticBlock_idx.frameNStart = frameN  # exact frame index
                semanticBlock_idx.tStart = t  # local t and not account for scr refresh
                semanticBlock_idx.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(semanticBlock_idx, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'semanticBlock_idx.started')
                # update status
                semanticBlock_idx.status = STARTED
                semanticBlock_idx.setAutoDraw(True)
            
            # if semanticBlock_idx is active this frame...
            if semanticBlock_idx.status == STARTED:
                # update params
                pass
            
            # *fixation* updates
            
            # if fixation is starting this frame...
            if fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation.frameNStart = frameN  # exact frame index
                fixation.tStart = t  # local t and not account for scr refresh
                fixation.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'fixation.started')
                # update status
                fixation.status = STARTED
                fixation.setAutoDraw(True)
            
            # if fixation is active this frame...
            if fixation.status == STARTED:
                # update params
                pass
            
            # *field_s* updates
            
            # if field_s is starting this frame...
            if field_s.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                field_s.frameNStart = frameN  # exact frame index
                field_s.tStart = t  # local t and not account for scr refresh
                field_s.tStartRefresh = tThisFlipGlobal  # on global time
                # add timestamp to datafile
                thisExp.addData('field_s.started', tThisFlipGlobal)
                # update status
                field_s.status = STARTED
                field_s.play(when=win)  # sync with win flip
            
            # if field_s is stopping this frame...
            if field_s.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > field_s.tStartRefresh + 2-frameTolerance or field_s.isFinished:
                    # keep track of stop time/frame for later
                    field_s.tStop = t  # not accounting for scr refresh
                    field_s.tStopRefresh = tThisFlipGlobal  # on global time
                    field_s.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'field_s.stopped')
                    # update status
                    field_s.status = FINISHED
                    field_s.stop()
            
            # *word1_s* updates
            
            # if word1_s is starting this frame...
            if word1_s.status == NOT_STARTED and tThisFlip >= 2-frameTolerance:
                # keep track of start time/frame for later
                word1_s.frameNStart = frameN  # exact frame index
                word1_s.tStart = t  # local t and not account for scr refresh
                word1_s.tStartRefresh = tThisFlipGlobal  # on global time
                # add timestamp to datafile
                thisExp.addData('word1_s.started', tThisFlipGlobal)
                # update status
                word1_s.status = STARTED
                word1_s.play(when=win)  # sync with win flip
            
            # if word1_s is stopping this frame...
            if word1_s.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > word1_s.tStartRefresh + 2-frameTolerance or word1_s.isFinished:
                    # keep track of stop time/frame for later
                    word1_s.tStop = t  # not accounting for scr refresh
                    word1_s.tStopRefresh = tThisFlipGlobal  # on global time
                    word1_s.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'word1_s.stopped')
                    # update status
                    word1_s.status = FINISHED
                    word1_s.stop()
            
            # *word2_s* updates
            
            # if word2_s is starting this frame...
            if word2_s.status == NOT_STARTED and tThisFlip >= 4-frameTolerance:
                # keep track of start time/frame for later
                word2_s.frameNStart = frameN  # exact frame index
                word2_s.tStart = t  # local t and not account for scr refresh
                word2_s.tStartRefresh = tThisFlipGlobal  # on global time
                # add timestamp to datafile
                thisExp.addData('word2_s.started', tThisFlipGlobal)
                # update status
                word2_s.status = STARTED
                word2_s.play(when=win)  # sync with win flip
            
            # if word2_s is stopping this frame...
            if word2_s.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > word2_s.tStartRefresh + 2-frameTolerance or word2_s.isFinished:
                    # keep track of stop time/frame for later
                    word2_s.tStop = t  # not accounting for scr refresh
                    word2_s.tStopRefresh = tThisFlipGlobal  # on global time
                    word2_s.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'word2_s.stopped')
                    # update status
                    word2_s.status = FINISHED
                    word2_s.stop()
            
            # *instr_s* updates
            
            # if instr_s is starting this frame...
            if instr_s.status == NOT_STARTED and tThisFlip >= 6-frameTolerance:
                # keep track of start time/frame for later
                instr_s.frameNStart = frameN  # exact frame index
                instr_s.tStart = t  # local t and not account for scr refresh
                instr_s.tStartRefresh = tThisFlipGlobal  # on global time
                # add timestamp to datafile
                thisExp.addData('instr_s.started', tThisFlipGlobal)
                # update status
                instr_s.status = STARTED
                instr_s.play(when=win)  # sync with win flip
            
            # if instr_s is stopping this frame...
            if instr_s.status == STARTED:
                if bool(False) or instr_s.isFinished:
                    # keep track of stop time/frame for later
                    instr_s.tStop = t  # not accounting for scr refresh
                    instr_s.tStopRefresh = tThisFlipGlobal  # on global time
                    instr_s.frameNStop = frameN  # exact frame index
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'instr_s.stopped')
                    # update status
                    instr_s.status = FINISHED
                    instr_s.stop()
            
            # *key_resp_vmi_s* updates
            waitOnFlip = False
            
            # if key_resp_vmi_s is starting this frame...
            if key_resp_vmi_s.status == NOT_STARTED and instr_s.status==FINISHED:
                # keep track of start time/frame for later
                key_resp_vmi_s.frameNStart = frameN  # exact frame index
                key_resp_vmi_s.tStart = t  # local t and not account for scr refresh
                key_resp_vmi_s.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(key_resp_vmi_s, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'key_resp_vmi_s.started')
                # update status
                key_resp_vmi_s.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(key_resp_vmi_s.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(key_resp_vmi_s.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if key_resp_vmi_s.status == STARTED and not waitOnFlip:
                theseKeys = key_resp_vmi_s.getKeys(keyList=['1','2'], ignoreKeys=None, waitRelease=False)
                _key_resp_vmi_s_allKeys.extend(theseKeys)
                if len(_key_resp_vmi_s_allKeys):
                    key_resp_vmi_s.keys = _key_resp_vmi_s_allKeys[-1].name  # just the last key pressed
                    key_resp_vmi_s.rt = _key_resp_vmi_s_allKeys[-1].rt
                    key_resp_vmi_s.duration = _key_resp_vmi_s_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, win=win)
                return
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[field_s, word1_s, word2_s, instr_s]
                )
                # skip the frame we paused on
                continue
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                sem_trial.forceEnded = routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in sem_trial.components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "sem_trial" ---
        for thisComponent in sem_trial.components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store stop times for sem_trial
        sem_trial.tStop = globalClock.getTime(format='float')
        sem_trial.tStopRefresh = tThisFlipGlobal
        thisExp.addData('sem_trial.stopped', sem_trial.tStop)
        field_s.pause()  # ensure sound has stopped at end of Routine
        word1_s.pause()  # ensure sound has stopped at end of Routine
        word2_s.pause()  # ensure sound has stopped at end of Routine
        instr_s.pause()  # ensure sound has stopped at end of Routine
        # check responses
        if key_resp_vmi_s.keys in ['', [], None]:  # No response was made
            key_resp_vmi_s.keys = None
        semanticBlock.addData('key_resp_vmi_s.keys',key_resp_vmi_s.keys)
        if key_resp_vmi_s.keys != None:  # we had a response
            semanticBlock.addData('key_resp_vmi_s.rt', key_resp_vmi_s.rt)
            semanticBlock.addData('key_resp_vmi_s.duration', key_resp_vmi_s.duration)
        # the Routine "sem_trial" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'semanticBlock'
    
    if thisSession is not None:
        # if running in a Session with a Liaison client, send data up to now
        thisSession.sendExperimentData()
    
    # --- Prepare to start Routine "end_session" ---
    # create an object to store info about Routine end_session
    end_session = data.Routine(
        name='end_session',
        components=[end_session_text],
    )
    end_session.status = NOT_STARTED
    continueRoutine = True
    # update component parameters for each repeat
    # store start times for end_session
    end_session.tStartRefresh = win.getFutureFlipTime(clock=globalClock)
    end_session.tStart = globalClock.getTime(format='float')
    end_session.status = STARTED
    thisExp.addData('end_session.started', end_session.tStart)
    end_session.maxDuration = None
    # keep track of which components have finished
    end_sessionComponents = end_session.components
    for thisComponent in end_session.components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "end_session" ---
    end_session.forceEnded = routineForceEnded = not continueRoutine
    while continueRoutine and routineTimer.getTime() < 6.0:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *end_session_text* updates
        
        # if end_session_text is starting this frame...
        if end_session_text.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            end_session_text.frameNStart = frameN  # exact frame index
            end_session_text.tStart = t  # local t and not account for scr refresh
            end_session_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(end_session_text, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'end_session_text.started')
            # update status
            end_session_text.status = STARTED
            end_session_text.setAutoDraw(True)
        
        # if end_session_text is active this frame...
        if end_session_text.status == STARTED:
            # update params
            pass
        
        # if end_session_text is stopping this frame...
        if end_session_text.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > end_session_text.tStartRefresh + 6-frameTolerance:
                # keep track of stop time/frame for later
                end_session_text.tStop = t  # not accounting for scr refresh
                end_session_text.tStopRefresh = tThisFlipGlobal  # on global time
                end_session_text.frameNStop = frameN  # exact frame index
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'end_session_text.stopped')
                # update status
                end_session_text.status = FINISHED
                end_session_text.setAutoDraw(False)
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, win=win)
            return
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
            )
            # skip the frame we paused on
            continue
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            end_session.forceEnded = routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in end_session.components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "end_session" ---
    for thisComponent in end_session.components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # store stop times for end_session
    end_session.tStop = globalClock.getTime(format='float')
    end_session.tStopRefresh = tThisFlipGlobal
    thisExp.addData('end_session.stopped', end_session.tStop)
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if end_session.maxDurationReached:
        routineTimer.addTime(-end_session.maxDuration)
    elif end_session.forceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-6.000000)
    thisExp.nextEntry()
    
    # mark experiment as finished
    endExperiment(thisExp, win=win)


def saveData(thisExp):
    """
    Save data from this experiment
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    filename = thisExp.dataFileName
    # these shouldn't be strictly necessary (should auto-save)
    thisExp.saveAsWideText(filename + '.csv', delim='auto')
    thisExp.saveAsPickle(filename)


def endExperiment(thisExp, win=None):
    """
    End this experiment, performing final shut down operations.
    
    This function does NOT close the window or end the Python process - use `quit` for this.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window for this experiment.
    """
    if win is not None:
        # remove autodraw from all current components
        win.clearAutoDraw()
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed
        win.flip()
    # return console logger level to WARNING
    logging.console.setLevel(logging.WARNING)
    # mark experiment handler as finished
    thisExp.status = FINISHED
    logging.flush()


def quit(thisExp, win=None, thisSession=None):
    """
    Fully quit, closing the window and ending the Python process.
    
    Parameters
    ==========
    win : psychopy.visual.Window
        Window to close.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    thisExp.abort()  # or data files will save again on exit
    # make sure everything is closed down
    if win is not None:
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed before quitting
        win.flip()
        win.close()
    logging.flush()
    if thisSession is not None:
        thisSession.stop()
    # terminate Python process
    core.quit()


# if running this experiment as a script...
if __name__ == '__main__':
    # call all functions in order
    expInfo = showExpInfoDlg(expInfo=expInfo)
    thisExp = setupData(expInfo=expInfo)
    logFile = setupLogging(filename=thisExp.dataFileName)
    win = setupWindow(expInfo=expInfo)
    setupDevices(expInfo=expInfo, thisExp=thisExp, win=win)
    run(
        expInfo=expInfo, 
        thisExp=thisExp, 
        win=win,
        globalClock='float'
    )
    saveData(thisExp=thisExp)
    quit(thisExp=thisExp, win=win)
