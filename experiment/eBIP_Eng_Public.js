/************************ 
 * Ebip_Eng_Public *
 ************************/

import { core, data, sound, util, visual, hardware } from './lib/psychojs-2024.2.4.js';
const { PsychoJS } = core;
const { TrialHandler, MultiStairHandler } = data;
const { Scheduler } = util;
//some handy aliases as in the psychopy scripts;
const { abs, sin, cos, PI: pi, sqrt } = Math;
const { round } = util;


// store info about the experiment session:
let expName = 'eBIP_Eng_Public';  // from the Builder filename that created this script
let expInfo = {
    'participant': 'Optional',
    'email': 'Optional@gmail.com',
    'gender (*)': 'Female',
    'age (*)': '25',
    'highest degree obtained (high school = 12) (*)': '12',
    'right or left handedness (*)': 'right',
    'profession (*)': 'profession',
};

// Start code blocks for 'Before Experiment'
// init psychoJS:
const psychoJS = new PsychoJS({
  debug: true
});

// open window:
psychoJS.openWindow({
  fullscr: true,
  color: new util.Color([1.0, 1.0, 1.0]),
  units: 'height',
  waitBlanking: true,
  backgroundImage: '',
  backgroundFit: 'none',
});
// schedule the experiment:
psychoJS.schedule(psychoJS.gui.DlgFromDict({
  dictionary: expInfo,
  title: expName
}));

const flowScheduler = new Scheduler(psychoJS);
const dialogCancelScheduler = new Scheduler(psychoJS);
psychoJS.scheduleCondition(function() { return (psychoJS.gui.dialogComponent.button === 'OK'); },flowScheduler, dialogCancelScheduler);

// flowScheduler gets run if the participants presses OK
flowScheduler.add(updateInfo); // add timeStamp
flowScheduler.add(experimentInit);
flowScheduler.add(resource_manageRoutineBegin());
flowScheduler.add(resource_manageRoutineEachFrame());
flowScheduler.add(resource_manageRoutineEnd());
flowScheduler.add(volume_testRoutineBegin());
flowScheduler.add(volume_testRoutineEachFrame());
flowScheduler.add(volume_testRoutineEnd());
flowScheduler.add(key_testRoutineBegin());
flowScheduler.add(key_testRoutineEachFrame());
flowScheduler.add(key_testRoutineEnd());
flowScheduler.add(begin_instructionRoutineBegin());
flowScheduler.add(begin_instructionRoutineEachFrame());
flowScheduler.add(begin_instructionRoutineEnd());
flowScheduler.add(vivid_instructionRoutineBegin());
flowScheduler.add(vivid_instructionRoutineEachFrame());
flowScheduler.add(vivid_instructionRoutineEnd());
flowScheduler.add(vviq_instructionRoutineBegin());
flowScheduler.add(vviq_instructionRoutineEachFrame());
flowScheduler.add(vviq_instructionRoutineEnd());
const vviq_trialsLoopScheduler = new Scheduler(psychoJS);
flowScheduler.add(vviq_trialsLoopBegin(vviq_trialsLoopScheduler));
flowScheduler.add(vviq_trialsLoopScheduler);
flowScheduler.add(vviq_trialsLoopEnd);


flowScheduler.add(vviq_feedbackRoutineBegin());
flowScheduler.add(vviq_feedbackRoutineEachFrame());
flowScheduler.add(vviq_feedbackRoutineEnd());
flowScheduler.add(imagery_instructionRoutineBegin());
flowScheduler.add(imagery_instructionRoutineEachFrame());
flowScheduler.add(imagery_instructionRoutineEnd());
const imagery_domainLoopScheduler = new Scheduler(psychoJS);
flowScheduler.add(imagery_domainLoopBegin(imagery_domainLoopScheduler));
flowScheduler.add(imagery_domainLoopScheduler);
flowScheduler.add(imagery_domainLoopEnd);













flowScheduler.add(perception_instructionRoutineBegin());
flowScheduler.add(perception_instructionRoutineEachFrame());
flowScheduler.add(perception_instructionRoutineEnd());
const perception_task_trialLoopScheduler = new Scheduler(psychoJS);
flowScheduler.add(perception_task_trialLoopBegin(perception_task_trialLoopScheduler));
flowScheduler.add(perception_task_trialLoopScheduler);
flowScheduler.add(perception_task_trialLoopEnd);




flowScheduler.add(perception_task_feedbackRoutineBegin());
flowScheduler.add(perception_task_feedbackRoutineEachFrame());
flowScheduler.add(perception_task_feedbackRoutineEnd());
flowScheduler.add(sem_instructionRoutineBegin());
flowScheduler.add(sem_instructionRoutineEachFrame());
flowScheduler.add(sem_instructionRoutineEnd());
const trialsLoopScheduler = new Scheduler(psychoJS);
flowScheduler.add(trialsLoopBegin(trialsLoopScheduler));
flowScheduler.add(trialsLoopScheduler);
flowScheduler.add(trialsLoopEnd);


const semanticBlockLoopScheduler = new Scheduler(psychoJS);
flowScheduler.add(semanticBlockLoopBegin(semanticBlockLoopScheduler));
flowScheduler.add(semanticBlockLoopScheduler);
flowScheduler.add(semanticBlockLoopEnd);


flowScheduler.add(end_sessionRoutineBegin());
flowScheduler.add(end_sessionRoutineEachFrame());
flowScheduler.add(end_sessionRoutineEnd());
flowScheduler.add(quitPsychoJS, '', true);

// quit if user presses Cancel in dialog box:
dialogCancelScheduler.add(quitPsychoJS, '', false);

psychoJS.start({
  expName: expName,
  expInfo: expInfo,
  resources: [
    // resources:
    {'name': 'vviq_task_online.xlsx', 'path': 'vviq_task_online.xlsx'},
    {'name': 'Imagery_domain_selection.xlsx', 'path': 'Imagery_domain_selection.xlsx'},
    {'name': 'ImageryPractice_en.xlsx', 'path': 'ImageryPractice_en.xlsx'},
    {'name': 'audioStims/Color.wav', 'path': 'audioStims/Color.wav'},
    {'name': 'audioStims/baguette.wav', 'path': 'audioStims/baguette.wav'},
    {'name': 'audioStims/cauliflower.wav', 'path': 'audioStims/cauliflower.wav'},
    {'name': 'audioStims/dark.wav', 'path': 'audioStims/dark.wav'},
    {'name': 'img/Colour_Trial.png', 'path': 'img/Colour_Trial.png'},
    {'name': 'audioStims/Face.wav', 'path': 'audioStims/Face.wav'},
    {'name': 'audioStims/rihanna.wav', 'path': 'audioStims/rihanna.wav'},
    {'name': 'audioStims/bradpitt.wav', 'path': 'audioStims/bradpitt.wav'},
    {'name': 'audioStims/long.wav', 'path': 'audioStims/long.wav'},
    {'name': 'img/Face_Trial.png', 'path': 'img/Face_Trial.png'},
    {'name': 'audioStims/Letter.wav', 'path': 'audioStims/Letter.wav'},
    {'name': 'audioStims/culture.wav', 'path': 'audioStims/culture.wav'},
    {'name': 'audioStims/prove.wav', 'path': 'audioStims/prove.wav'},
    {'name': 'audioStims/down.wav', 'path': 'audioStims/down.wav'},
    {'name': 'img/Letter_Trial.png', 'path': 'img/Letter_Trial.png'},
    {'name': 'audioStims/Map.wav', 'path': 'audioStims/Map.wav'},
    {'name': 'audioStims/washingtondc.wav', 'path': 'audioStims/washingtondc.wav'},
    {'name': 'audioStims/houston.wav', 'path': 'audioStims/houston.wav'},
    {'name': 'audioStims/left.wav', 'path': 'audioStims/left.wav'},
    {'name': 'img/Map_Trial.png', 'path': 'img/Map_Trial.png'},
    {'name': 'audioStims/Shape.wav', 'path': 'audioStims/Shape.wav'},
    {'name': 'audioStims/broom.wav', 'path': 'audioStims/broom.wav'},
    {'name': 'audioStims/bucket.wav', 'path': 'audioStims/bucket.wav'},
    {'name': 'img/Shape_Trial.png', 'path': 'img/Shape_Trial.png'},
    {'name': 'ImageryItems_en.xlsx', 'path': 'ImageryItems_en.xlsx'},
    {'name': 'audioStims/Color.wav', 'path': 'audioStims/Color.wav'},
    {'name': 'audioStims/marshmellow.wav', 'path': 'audioStims/marshmellow.wav'},
    {'name': 'audioStims/baguette.wav', 'path': 'audioStims/baguette.wav'},
    {'name': 'audioStims/light.wav', 'path': 'audioStims/light.wav'},
    {'name': 'audioStims/strawberry.wav', 'path': 'audioStims/strawberry.wav'},
    {'name': 'audioStims/fig.wav', 'path': 'audioStims/fig.wav'},
    {'name': 'audioStims/redwine.wav', 'path': 'audioStims/redwine.wav'},
    {'name': 'audioStims/espresso.wav', 'path': 'audioStims/espresso.wav'},
    {'name': 'audioStims/dark.wav', 'path': 'audioStims/dark.wav'},
    {'name': 'audioStims/pumpkin.wav', 'path': 'audioStims/pumpkin.wav'},
    {'name': 'audioStims/banana.wav', 'path': 'audioStims/banana.wav'},
    {'name': 'audioStims/guacamole.wav', 'path': 'audioStims/guacamole.wav'},
    {'name': 'audioStims/pomogranate.wav', 'path': 'audioStims/pomogranate.wav'},
    {'name': 'audioStims/greenpepper.wav', 'path': 'audioStims/greenpepper.wav'},
    {'name': 'audioStims/zucchini.wav', 'path': 'audioStims/zucchini.wav'},
    {'name': 'audioStims/lemon.wav', 'path': 'audioStims/lemon.wav'},
    {'name': 'audioStims/pineapple.wav', 'path': 'audioStims/pineapple.wav'},
    {'name': 'audioStims/cucumber.wav', 'path': 'audioStims/cucumber.wav'},
    {'name': 'audioStims/artichokes.wav', 'path': 'audioStims/artichokes.wav'},
    {'name': 'audioStims/cauliflower.wav', 'path': 'audioStims/cauliflower.wav'},
    {'name': 'audioStims/greenapple.wav', 'path': 'audioStims/greenapple.wav'},
    {'name': 'audioStims/Face.wav', 'path': 'audioStims/Face.wav'},
    {'name': 'audioStims/michaelphelps.wav', 'path': 'audioStims/michaelphelps.wav'},
    {'name': 'audioStims/justintimberlake.wav', 'path': 'audioStims/justintimberlake.wav'},
    {'name': 'audioStims/long.wav', 'path': 'audioStims/long.wav'},
    {'name': 'audioStims/oprahwinfrey.wav', 'path': 'audioStims/oprahwinfrey.wav'},
    {'name': 'audioStims/kevinhart.wav', 'path': 'audioStims/kevinhart.wav'},
    {'name': 'audioStims/round.wav', 'path': 'audioStims/round.wav'},
    {'name': 'audioStims/taylorswift.wav', 'path': 'audioStims/taylorswift.wav'},
    {'name': 'audioStims/leonardodicaprio.wav', 'path': 'audioStims/leonardodicaprio.wav'},
    {'name': 'audioStims/kamalaharris.wav', 'path': 'audioStims/kamalaharris.wav'},
    {'name': 'audioStims/princewilliam.wav', 'path': 'audioStims/princewilliam.wav'},
    {'name': 'audioStims/juliaroberts.wav', 'path': 'audioStims/juliaroberts.wav'},
    {'name': 'audioStims/sarahjessicaparker.wav', 'path': 'audioStims/sarahjessicaparker.wav'},
    {'name': 'audioStims/matthewmcconaughey.wav', 'path': 'audioStims/matthewmcconaughey.wav'},
    {'name': 'audioStims/georgewbush.wav', 'path': 'audioStims/georgewbush.wav'},
    {'name': 'audioStims/rihanna.wav', 'path': 'audioStims/rihanna.wav'},
    {'name': 'audioStims/stevejobs.wav', 'path': 'audioStims/stevejobs.wav'},
    {'name': 'audioStims/willsmith.wav', 'path': 'audioStims/willsmith.wav'},
    {'name': 'audioStims/shakira.wav', 'path': 'audioStims/shakira.wav'},
    {'name': 'audioStims/emmastone.wav', 'path': 'audioStims/emmastone.wav'},
    {'name': 'audioStims/barackobama.wav', 'path': 'audioStims/barackobama.wav'},
    {'name': 'audioStims/johnnydepp.wav', 'path': 'audioStims/johnnydepp.wav'},
    {'name': 'audioStims/serenawilliams.wav', 'path': 'audioStims/serenawilliams.wav'},
    {'name': 'audioStims/bradpitt.wav', 'path': 'audioStims/bradpitt.wav'},
    {'name': 'audioStims/selenagomez.wav', 'path': 'audioStims/selenagomez.wav'},
    {'name': 'audioStims/markzuckerberg.wav', 'path': 'audioStims/markzuckerberg.wav'},
    {'name': 'audioStims/Letter.wav', 'path': 'audioStims/Letter.wav'},
    {'name': 'audioStims/prove.wav', 'path': 'audioStims/prove.wav'},
    {'name': 'audioStims/silence.wav', 'path': 'audioStims/silence.wav'},
    {'name': 'audioStims/up.wav', 'path': 'audioStims/up.wav'},
    {'name': 'audioStims/reject.wav', 'path': 'audioStims/reject.wav'},
    {'name': 'audioStims/rigor.wav', 'path': 'audioStims/rigor.wav'},
    {'name': 'audioStims/justice.wav', 'path': 'audioStims/justice.wav'},
    {'name': 'audioStims/impression.wav', 'path': 'audioStims/impression.wav'},
    {'name': 'audioStims/opinion.wav', 'path': 'audioStims/opinion.wav'},
    {'name': 'audioStims/sentence.wav', 'path': 'audioStims/sentence.wav'},
    {'name': 'audioStims/down.wav', 'path': 'audioStims/down.wav'},
    {'name': 'audioStims/warmth.wav', 'path': 'audioStims/warmth.wav'},
    {'name': 'audioStims/year.wav', 'path': 'audioStims/year.wav'},
    {'name': 'audioStims/mention.wav', 'path': 'audioStims/mention.wav'},
    {'name': 'audioStims/spirit.wav', 'path': 'audioStims/spirit.wav'},
    {'name': 'audioStims/culture.wav', 'path': 'audioStims/culture.wav'},
    {'name': 'audioStims/reasoning.wav', 'path': 'audioStims/reasoning.wav'},
    {'name': 'audioStims/merit.wav', 'path': 'audioStims/merit.wav'},
    {'name': 'audioStims/summary.wav', 'path': 'audioStims/summary.wav'},
    {'name': 'audioStims/evocation.wav', 'path': 'audioStims/evocation.wav'},
    {'name': 'audioStims/retrieve.wav', 'path': 'audioStims/retrieve.wav'},
    {'name': 'audioStims/brilliance.wav', 'path': 'audioStims/brilliance.wav'},
    {'name': 'audioStims/cold.wav', 'path': 'audioStims/cold.wav'},
    {'name': 'audioStims/bad.wav', 'path': 'audioStims/bad.wav'},
    {'name': 'audioStims/decision.wav', 'path': 'audioStims/decision.wav'},
    {'name': 'audioStims/altruism.wav', 'path': 'audioStims/altruism.wav'},
    {'name': 'audioStims/smart.wav', 'path': 'audioStims/smart.wav'},
    {'name': 'audioStims/Map.wav', 'path': 'audioStims/Map.wav'},
    {'name': 'audioStims/cincinnati.wav', 'path': 'audioStims/cincinnati.wav'},
    {'name': 'audioStims/austin.wav', 'path': 'audioStims/austin.wav'},
    {'name': 'audioStims/left.wav', 'path': 'audioStims/left.wav'},
    {'name': 'audioStims/seattle.wav', 'path': 'audioStims/seattle.wav'},
    {'name': 'audioStims/detroit.wav', 'path': 'audioStims/detroit.wav'},
    {'name': 'audioStims/right.wav', 'path': 'audioStims/right.wav'},
    {'name': 'audioStims/washingtondc.wav', 'path': 'audioStims/washingtondc.wav'},
    {'name': 'audioStims/houston.wav', 'path': 'audioStims/houston.wav'},
    {'name': 'audioStims/baltimore.wav', 'path': 'audioStims/baltimore.wav'},
    {'name': 'audioStims/dallas.wav', 'path': 'audioStims/dallas.wav'},
    {'name': 'audioStims/phoenix.wav', 'path': 'audioStims/phoenix.wav'},
    {'name': 'audioStims/minneapolis.wav', 'path': 'audioStims/minneapolis.wav'},
    {'name': 'audioStims/nashville.wav', 'path': 'audioStims/nashville.wav'},
    {'name': 'audioStims/kansascity.wav', 'path': 'audioStims/kansascity.wav'},
    {'name': 'audioStims/atlanta.wav', 'path': 'audioStims/atlanta.wav'},
    {'name': 'audioStims/lasvegas.wav', 'path': 'audioStims/lasvegas.wav'},
    {'name': 'audioStims/philadelphia.wav', 'path': 'audioStims/philadelphia.wav'},
    {'name': 'audioStims/atlanticcity.wav', 'path': 'audioStims/atlanticcity.wav'},
    {'name': 'audioStims/boston.wav', 'path': 'audioStims/boston.wav'},
    {'name': 'audioStims/memphis.wav', 'path': 'audioStims/memphis.wav'},
    {'name': 'audioStims/miami.wav', 'path': 'audioStims/miami.wav'},
    {'name': 'audioStims/Shape.wav', 'path': 'audioStims/Shape.wav'},
    {'name': 'audioStims/bucket.wav', 'path': 'audioStims/bucket.wav'},
    {'name': 'audioStims/broom.wav', 'path': 'audioStims/broom.wav'},
    {'name': 'audioStims/pan.wav', 'path': 'audioStims/pan.wav'},
    {'name': 'audioStims/bottle.wav', 'path': 'audioStims/bottle.wav'},
    {'name': 'audioStims/bathtub.wav', 'path': 'audioStims/bathtub.wav'},
    {'name': 'audioStims/tripod.wav', 'path': 'audioStims/tripod.wav'},
    {'name': 'audioStims/whiteshark.wav', 'path': 'audioStims/whiteshark.wav'},
    {'name': 'audioStims/ladybug.wav', 'path': 'audioStims/ladybug.wav'},
    {'name': 'audioStims/flamingo.wav', 'path': 'audioStims/flamingo.wav'},
    {'name': 'audioStims/chicken.wav', 'path': 'audioStims/chicken.wav'},
    {'name': 'audioStims/koala.wav', 'path': 'audioStims/koala.wav'},
    {'name': 'audioStims/penguin.wav', 'path': 'audioStims/penguin.wav'},
    {'name': 'audioStims/hamster.wav', 'path': 'audioStims/hamster.wav'},
    {'name': 'audioStims/kangarou.wav', 'path': 'audioStims/kangarou.wav'},
    {'name': 'audioStims/lighter.wav', 'path': 'audioStims/lighter.wav'},
    {'name': 'audioStims/bowl.wav', 'path': 'audioStims/bowl.wav'},
    {'name': 'audioStims/panda.wav', 'path': 'audioStims/panda.wav'},
    {'name': 'PerceptionItems_en.xlsx', 'path': 'PerceptionItems_en.xlsx'},
    {'name': 'audioStims/Shape.wav', 'path': 'audioStims/Shape.wav'},
    {'name': 'audioStims/hamster.wav', 'path': 'audioStims/hamster.wav'},
    {'name': 'audioStims/kangarou.wav', 'path': 'audioStims/kangarou.wav'},
    {'name': 'audioStims/round.wav', 'path': 'audioStims/round.wav'},
    {'name': 'imgPerception/Slide1.png', 'path': 'imgPerception/Slide1.png'},
    {'name': 'imgPerception/Slide2.png', 'path': 'imgPerception/Slide2.png'},
    {'name': 'imgPerception/Slide3.png', 'path': 'imgPerception/Slide3.png'},
    {'name': 'audioStims/bucket.wav', 'path': 'audioStims/bucket.wav'},
    {'name': 'audioStims/broom.wav', 'path': 'audioStims/broom.wav'},
    {'name': 'audioStims/long.wav', 'path': 'audioStims/long.wav'},
    {'name': 'imgPerception/Slide4.png', 'path': 'imgPerception/Slide4.png'},
    {'name': 'imgPerception/Slide5.png', 'path': 'imgPerception/Slide5.png'},
    {'name': 'imgPerception/Slide6.png', 'path': 'imgPerception/Slide6.png'},
    {'name': 'audioStims/bottle.wav', 'path': 'audioStims/bottle.wav'},
    {'name': 'audioStims/pan.wav', 'path': 'audioStims/pan.wav'},
    {'name': 'imgPerception/Slide7.png', 'path': 'imgPerception/Slide7.png'},
    {'name': 'imgPerception/Slide8.png', 'path': 'imgPerception/Slide8.png'},
    {'name': 'imgPerception/Slide9.png', 'path': 'imgPerception/Slide9.png'},
    {'name': 'audioStims/bathtub.wav', 'path': 'audioStims/bathtub.wav'},
    {'name': 'audioStims/tripod.wav', 'path': 'audioStims/tripod.wav'},
    {'name': 'imgPerception/Slide10.png', 'path': 'imgPerception/Slide10.png'},
    {'name': 'imgPerception/Slide11.png', 'path': 'imgPerception/Slide11.png'},
    {'name': 'imgPerception/Slide12.png', 'path': 'imgPerception/Slide12.png'},
    {'name': 'audioStims/whiteshark.wav', 'path': 'audioStims/whiteshark.wav'},
    {'name': 'audioStims/ladybug.wav', 'path': 'audioStims/ladybug.wav'},
    {'name': 'imgPerception/Slide13.png', 'path': 'imgPerception/Slide13.png'},
    {'name': 'imgPerception/Slide14.png', 'path': 'imgPerception/Slide14.png'},
    {'name': 'imgPerception/Slide15.png', 'path': 'imgPerception/Slide15.png'},
    {'name': 'audioStims/flamingo.wav', 'path': 'audioStims/flamingo.wav'},
    {'name': 'audioStims/chicken.wav', 'path': 'audioStims/chicken.wav'},
    {'name': 'imgPerception/Slide16.png', 'path': 'imgPerception/Slide16.png'},
    {'name': 'imgPerception/Slide17.png', 'path': 'imgPerception/Slide17.png'},
    {'name': 'imgPerception/Slide18.png', 'path': 'imgPerception/Slide18.png'},
    {'name': 'audioStims/koala.wav', 'path': 'audioStims/koala.wav'},
    {'name': 'audioStims/penguin.wav', 'path': 'audioStims/penguin.wav'},
    {'name': 'imgPerception/Slide19.png', 'path': 'imgPerception/Slide19.png'},
    {'name': 'imgPerception/Slide20.png', 'path': 'imgPerception/Slide20.png'},
    {'name': 'imgPerception/Slide21.png', 'path': 'imgPerception/Slide21.png'},
    {'name': 'audioStims/panda.wav', 'path': 'audioStims/panda.wav'},
    {'name': 'imgPerception/Slide22.png', 'path': 'imgPerception/Slide22.png'},
    {'name': 'imgPerception/Slide23.png', 'path': 'imgPerception/Slide23.png'},
    {'name': 'imgPerception/Slide24.png', 'path': 'imgPerception/Slide24.png'},
    {'name': 'audioStims/lighter.wav', 'path': 'audioStims/lighter.wav'},
    {'name': 'audioStims/bowl.wav', 'path': 'audioStims/bowl.wav'},
    {'name': 'imgPerception/Slide25.png', 'path': 'imgPerception/Slide25.png'},
    {'name': 'imgPerception/Slide26.png', 'path': 'imgPerception/Slide26.png'},
    {'name': 'imgPerception/Slide27.png', 'path': 'imgPerception/Slide27.png'},
    {'name': 'imgPerception/Slide28.png', 'path': 'imgPerception/Slide28.png'},
    {'name': 'imgPerception/Slide29.png', 'path': 'imgPerception/Slide29.png'},
    {'name': 'imgPerception/Slide30.png', 'path': 'imgPerception/Slide30.png'},
    {'name': 'imgPerception/Slide31.png', 'path': 'imgPerception/Slide31.png'},
    {'name': 'imgPerception/Slide32.png', 'path': 'imgPerception/Slide32.png'},
    {'name': 'imgPerception/Slide33.png', 'path': 'imgPerception/Slide33.png'},
    {'name': 'imgPerception/Slide34.png', 'path': 'imgPerception/Slide34.png'},
    {'name': 'imgPerception/Slide35.png', 'path': 'imgPerception/Slide35.png'},
    {'name': 'imgPerception/Slide36.png', 'path': 'imgPerception/Slide36.png'},
    {'name': 'audioStims/Color.wav', 'path': 'audioStims/Color.wav'},
    {'name': 'audioStims/artichokes.wav', 'path': 'audioStims/artichokes.wav'},
    {'name': 'audioStims/zucchini.wav', 'path': 'audioStims/zucchini.wav'},
    {'name': 'audioStims/light.wav', 'path': 'audioStims/light.wav'},
    {'name': 'imgPerception/Slide37.png', 'path': 'imgPerception/Slide37.png'},
    {'name': 'imgPerception/Slide38.png', 'path': 'imgPerception/Slide38.png'},
    {'name': 'imgPerception/Slide39.png', 'path': 'imgPerception/Slide39.png'},
    {'name': 'audioStims/pomogranate.wav', 'path': 'audioStims/pomogranate.wav'},
    {'name': 'audioStims/strawberry.wav', 'path': 'audioStims/strawberry.wav'},
    {'name': 'audioStims/dark.wav', 'path': 'audioStims/dark.wav'},
    {'name': 'imgPerception/Slide40.png', 'path': 'imgPerception/Slide40.png'},
    {'name': 'imgPerception/Slide41.png', 'path': 'imgPerception/Slide41.png'},
    {'name': 'imgPerception/Slide42.png', 'path': 'imgPerception/Slide42.png'},
    {'name': 'audioStims/guacamole.wav', 'path': 'audioStims/guacamole.wav'},
    {'name': 'imgPerception/Slide43.png', 'path': 'imgPerception/Slide43.png'},
    {'name': 'imgPerception/Slide44.png', 'path': 'imgPerception/Slide44.png'},
    {'name': 'imgPerception/Slide45.png', 'path': 'imgPerception/Slide45.png'},
    {'name': 'audioStims/baguette.wav', 'path': 'audioStims/baguette.wav'},
    {'name': 'audioStims/cauliflower.wav', 'path': 'audioStims/cauliflower.wav'},
    {'name': 'imgPerception/Slide46.png', 'path': 'imgPerception/Slide46.png'},
    {'name': 'imgPerception/Slide47.png', 'path': 'imgPerception/Slide47.png'},
    {'name': 'imgPerception/Slide48.png', 'path': 'imgPerception/Slide48.png'},
    {'name': 'audioStims/fig.wav', 'path': 'audioStims/fig.wav'},
    {'name': 'audioStims/pumpkin.wav', 'path': 'audioStims/pumpkin.wav'},
    {'name': 'imgPerception/Slide49.png', 'path': 'imgPerception/Slide49.png'},
    {'name': 'imgPerception/Slide50.png', 'path': 'imgPerception/Slide50.png'},
    {'name': 'imgPerception/Slide51.png', 'path': 'imgPerception/Slide51.png'},
    {'name': 'audioStims/espresso.wav', 'path': 'audioStims/espresso.wav'},
    {'name': 'audioStims/burrito.wav', 'path': 'audioStims/burrito.wav'},
    {'name': 'imgPerception/Slide52.png', 'path': 'imgPerception/Slide52.png'},
    {'name': 'imgPerception/Slide53.png', 'path': 'imgPerception/Slide53.png'},
    {'name': 'imgPerception/Slide54.png', 'path': 'imgPerception/Slide54.png'},
    {'name': 'audioStims/banana.wav', 'path': 'audioStims/banana.wav'},
    {'name': 'audioStims/redwine.wav', 'path': 'audioStims/redwine.wav'},
    {'name': 'imgPerception/Slide55.png', 'path': 'imgPerception/Slide55.png'},
    {'name': 'imgPerception/Slide56.png', 'path': 'imgPerception/Slide56.png'},
    {'name': 'imgPerception/Slide57.png', 'path': 'imgPerception/Slide57.png'},
    {'name': 'audioStims/lemon.wav', 'path': 'audioStims/lemon.wav'},
    {'name': 'audioStims/pineapple.wav', 'path': 'audioStims/pineapple.wav'},
    {'name': 'imgPerception/Slide58.png', 'path': 'imgPerception/Slide58.png'},
    {'name': 'imgPerception/Slide59.png', 'path': 'imgPerception/Slide59.png'},
    {'name': 'imgPerception/Slide60.png', 'path': 'imgPerception/Slide60.png'},
    {'name': 'imgPerception/Slide61.png', 'path': 'imgPerception/Slide61.png'},
    {'name': 'imgPerception/Slide62.png', 'path': 'imgPerception/Slide62.png'},
    {'name': 'imgPerception/Slide63.png', 'path': 'imgPerception/Slide63.png'},
    {'name': 'imgPerception/Slide64.png', 'path': 'imgPerception/Slide64.png'},
    {'name': 'imgPerception/Slide65.png', 'path': 'imgPerception/Slide65.png'},
    {'name': 'imgPerception/Slide66.png', 'path': 'imgPerception/Slide66.png'},
    {'name': 'audioStims/greenpepper.wav', 'path': 'audioStims/greenpepper.wav'},
    {'name': 'imgPerception/Slide67.png', 'path': 'imgPerception/Slide67.png'},
    {'name': 'imgPerception/Slide68.png', 'path': 'imgPerception/Slide68.png'},
    {'name': 'imgPerception/Slide69.png', 'path': 'imgPerception/Slide69.png'},
    {'name': 'imgPerception/Slide70.png', 'path': 'imgPerception/Slide70.png'},
    {'name': 'imgPerception/Slide71.png', 'path': 'imgPerception/Slide71.png'},
    {'name': 'imgPerception/Slide72.png', 'path': 'imgPerception/Slide72.png'},
    {'name': 'audioStims/Letter.wav', 'path': 'audioStims/Letter.wav'},
    {'name': 'audioStims/prove.wav', 'path': 'audioStims/prove.wav'},
    {'name': 'audioStims/silence.wav', 'path': 'audioStims/silence.wav'},
    {'name': 'audioStims/up.wav', 'path': 'audioStims/up.wav'},
    {'name': 'imgPerception/Slide73.png', 'path': 'imgPerception/Slide73.png'},
    {'name': 'imgPerception/Slide74.png', 'path': 'imgPerception/Slide74.png'},
    {'name': 'imgPerception/Slide75.png', 'path': 'imgPerception/Slide75.png'},
    {'name': 'audioStims/reject.wav', 'path': 'audioStims/reject.wav'},
    {'name': 'audioStims/rigor.wav', 'path': 'audioStims/rigor.wav'},
    {'name': 'imgPerception/Slide76.png', 'path': 'imgPerception/Slide76.png'},
    {'name': 'imgPerception/Slide77.png', 'path': 'imgPerception/Slide77.png'},
    {'name': 'imgPerception/Slide78.png', 'path': 'imgPerception/Slide78.png'},
    {'name': 'audioStims/justice.wav', 'path': 'audioStims/justice.wav'},
    {'name': 'audioStims/impression.wav', 'path': 'audioStims/impression.wav'},
    {'name': 'imgPerception/Slide79.png', 'path': 'imgPerception/Slide79.png'},
    {'name': 'imgPerception/Slide80.png', 'path': 'imgPerception/Slide80.png'},
    {'name': 'imgPerception/Slide81.png', 'path': 'imgPerception/Slide81.png'},
    {'name': 'audioStims/opinion.wav', 'path': 'audioStims/opinion.wav'},
    {'name': 'audioStims/sentence.wav', 'path': 'audioStims/sentence.wav'},
    {'name': 'audioStims/down.wav', 'path': 'audioStims/down.wav'},
    {'name': 'imgPerception/Slide82.png', 'path': 'imgPerception/Slide82.png'},
    {'name': 'imgPerception/Slide83.png', 'path': 'imgPerception/Slide83.png'},
    {'name': 'imgPerception/Slide84.png', 'path': 'imgPerception/Slide84.png'},
    {'name': 'audioStims/warmth.wav', 'path': 'audioStims/warmth.wav'},
    {'name': 'imgPerception/Slide85.png', 'path': 'imgPerception/Slide85.png'},
    {'name': 'imgPerception/Slide86.png', 'path': 'imgPerception/Slide86.png'},
    {'name': 'imgPerception/Slide87.png', 'path': 'imgPerception/Slide87.png'},
    {'name': 'audioStims/year.wav', 'path': 'audioStims/year.wav'},
    {'name': 'audioStims/mention.wav', 'path': 'audioStims/mention.wav'},
    {'name': 'imgPerception/Slide88.png', 'path': 'imgPerception/Slide88.png'},
    {'name': 'imgPerception/Slide89.png', 'path': 'imgPerception/Slide89.png'},
    {'name': 'imgPerception/Slide90.png', 'path': 'imgPerception/Slide90.png'},
    {'name': 'audioStims/spirit.wav', 'path': 'audioStims/spirit.wav'},
    {'name': 'audioStims/culture.wav', 'path': 'audioStims/culture.wav'},
    {'name': 'imgPerception/Slide91.png', 'path': 'imgPerception/Slide91.png'},
    {'name': 'imgPerception/Slide92.png', 'path': 'imgPerception/Slide92.png'},
    {'name': 'imgPerception/Slide93.png', 'path': 'imgPerception/Slide93.png'},
    {'name': 'audioStims/merit.wav', 'path': 'audioStims/merit.wav'},
    {'name': 'imgPerception/Slide94.png', 'path': 'imgPerception/Slide94.png'},
    {'name': 'imgPerception/Slide95.png', 'path': 'imgPerception/Slide95.png'},
    {'name': 'imgPerception/Slide96.png', 'path': 'imgPerception/Slide96.png'},
    {'name': 'audioStims/summary.wav', 'path': 'audioStims/summary.wav'},
    {'name': 'audioStims/evocation.wav', 'path': 'audioStims/evocation.wav'},
    {'name': 'imgPerception/Slide97.png', 'path': 'imgPerception/Slide97.png'},
    {'name': 'imgPerception/Slide98.png', 'path': 'imgPerception/Slide98.png'},
    {'name': 'imgPerception/Slide99.png', 'path': 'imgPerception/Slide99.png'},
    {'name': 'imgPerception/Slide100.png', 'path': 'imgPerception/Slide100.png'},
    {'name': 'imgPerception/Slide101.png', 'path': 'imgPerception/Slide101.png'},
    {'name': 'imgPerception/Slide102.png', 'path': 'imgPerception/Slide102.png'},
    {'name': 'audioStims/retrieve.wav', 'path': 'audioStims/retrieve.wav'},
    {'name': 'imgPerception/Slide103.png', 'path': 'imgPerception/Slide103.png'},
    {'name': 'imgPerception/Slide104.png', 'path': 'imgPerception/Slide104.png'},
    {'name': 'imgPerception/Slide105.png', 'path': 'imgPerception/Slide105.png'},
    {'name': 'audioStims/brilliance.wav', 'path': 'audioStims/brilliance.wav'},
    {'name': 'imgPerception/Slide106.png', 'path': 'imgPerception/Slide106.png'},
    {'name': 'imgPerception/Slide107.png', 'path': 'imgPerception/Slide107.png'},
    {'name': 'imgPerception/Slide108.png', 'path': 'imgPerception/Slide108.png'},
    {'name': 'audioStims/Map.wav', 'path': 'audioStims/Map.wav'},
    {'name': 'audioStims/cincinnati.wav', 'path': 'audioStims/cincinnati.wav'},
    {'name': 'audioStims/austin.wav', 'path': 'audioStims/austin.wav'},
    {'name': 'audioStims/left.wav', 'path': 'audioStims/left.wav'},
    {'name': 'imgPerception/Slide109.png', 'path': 'imgPerception/Slide109.png'},
    {'name': 'imgPerception/Slide110.png', 'path': 'imgPerception/Slide110.png'},
    {'name': 'imgPerception/Slide111.png', 'path': 'imgPerception/Slide111.png'},
    {'name': 'audioStims/seattle.wav', 'path': 'audioStims/seattle.wav'},
    {'name': 'audioStims/detroit.wav', 'path': 'audioStims/detroit.wav'},
    {'name': 'audioStims/right.wav', 'path': 'audioStims/right.wav'},
    {'name': 'imgPerception/Slide112.png', 'path': 'imgPerception/Slide112.png'},
    {'name': 'imgPerception/Slide113.png', 'path': 'imgPerception/Slide113.png'},
    {'name': 'imgPerception/Slide114.png', 'path': 'imgPerception/Slide114.png'},
    {'name': 'audioStims/washingtondc.wav', 'path': 'audioStims/washingtondc.wav'},
    {'name': 'audioStims/houston.wav', 'path': 'audioStims/houston.wav'},
    {'name': 'imgPerception/Slide115.png', 'path': 'imgPerception/Slide115.png'},
    {'name': 'imgPerception/Slide116.png', 'path': 'imgPerception/Slide116.png'},
    {'name': 'imgPerception/Slide117.png', 'path': 'imgPerception/Slide117.png'},
    {'name': 'audioStims/baltimore.wav', 'path': 'audioStims/baltimore.wav'},
    {'name': 'audioStims/dallas.wav', 'path': 'audioStims/dallas.wav'},
    {'name': 'imgPerception/Slide118.png', 'path': 'imgPerception/Slide118.png'},
    {'name': 'imgPerception/Slide119.png', 'path': 'imgPerception/Slide119.png'},
    {'name': 'imgPerception/Slide120.png', 'path': 'imgPerception/Slide120.png'},
    {'name': 'audioStims/phoenix.wav', 'path': 'audioStims/phoenix.wav'},
    {'name': 'imgPerception/Slide121.png', 'path': 'imgPerception/Slide121.png'},
    {'name': 'imgPerception/Slide122.png', 'path': 'imgPerception/Slide122.png'},
    {'name': 'imgPerception/Slide123.png', 'path': 'imgPerception/Slide123.png'},
    {'name': 'audioStims/minneapolis.wav', 'path': 'audioStims/minneapolis.wav'},
    {'name': 'audioStims/nashville.wav', 'path': 'audioStims/nashville.wav'},
    {'name': 'imgPerception/Slide124.png', 'path': 'imgPerception/Slide124.png'},
    {'name': 'imgPerception/Slide125.png', 'path': 'imgPerception/Slide125.png'},
    {'name': 'imgPerception/Slide126.png', 'path': 'imgPerception/Slide126.png'},
    {'name': 'audioStims/kansascity.wav', 'path': 'audioStims/kansascity.wav'},
    {'name': 'audioStims/atlanta.wav', 'path': 'audioStims/atlanta.wav'},
    {'name': 'imgPerception/Slide127.png', 'path': 'imgPerception/Slide127.png'},
    {'name': 'imgPerception/Slide128.png', 'path': 'imgPerception/Slide128.png'},
    {'name': 'imgPerception/Slide129.png', 'path': 'imgPerception/Slide129.png'},
    {'name': 'audioStims/lasvegas.wav', 'path': 'audioStims/lasvegas.wav'},
    {'name': 'audioStims/philadelphia.wav', 'path': 'audioStims/philadelphia.wav'},
    {'name': 'imgPerception/Slide130.png', 'path': 'imgPerception/Slide130.png'},
    {'name': 'imgPerception/Slide131.png', 'path': 'imgPerception/Slide131.png'},
    {'name': 'imgPerception/Slide132.png', 'path': 'imgPerception/Slide132.png'},
    {'name': 'audioStims/atlanticcity.wav', 'path': 'audioStims/atlanticcity.wav'},
    {'name': 'audioStims/memphis.wav', 'path': 'audioStims/memphis.wav'},
    {'name': 'imgPerception/Slide133.png', 'path': 'imgPerception/Slide133.png'},
    {'name': 'imgPerception/Slide134.png', 'path': 'imgPerception/Slide134.png'},
    {'name': 'imgPerception/Slide135.png', 'path': 'imgPerception/Slide135.png'},
    {'name': 'imgPerception/Slide136.png', 'path': 'imgPerception/Slide136.png'},
    {'name': 'imgPerception/Slide137.png', 'path': 'imgPerception/Slide137.png'},
    {'name': 'imgPerception/Slide138.png', 'path': 'imgPerception/Slide138.png'},
    {'name': 'audioStims/portland.wav', 'path': 'audioStims/portland.wav'},
    {'name': 'imgPerception/Slide139.png', 'path': 'imgPerception/Slide139.png'},
    {'name': 'imgPerception/Slide140.png', 'path': 'imgPerception/Slide140.png'},
    {'name': 'imgPerception/Slide141.png', 'path': 'imgPerception/Slide141.png'},
    {'name': 'imgPerception/Slide142.png', 'path': 'imgPerception/Slide142.png'},
    {'name': 'imgPerception/Slide143.png', 'path': 'imgPerception/Slide143.png'},
    {'name': 'imgPerception/Slide144.png', 'path': 'imgPerception/Slide144.png'},
    {'name': 'audioStims/Face.wav', 'path': 'audioStims/Face.wav'},
    {'name': 'audioStims/justintimberlake.wav', 'path': 'audioStims/justintimberlake.wav'},
    {'name': 'audioStims/matthewmcconaughey.wav', 'path': 'audioStims/matthewmcconaughey.wav'},
    {'name': 'imgPerception/Slide145.png', 'path': 'imgPerception/Slide145.png'},
    {'name': 'imgPerception/Slide146.png', 'path': 'imgPerception/Slide146.png'},
    {'name': 'imgPerception/Slide147.png', 'path': 'imgPerception/Slide147.png'},
    {'name': 'audioStims/kevinhart.wav', 'path': 'audioStims/kevinhart.wav'},
    {'name': 'audioStims/elonmusk.wav', 'path': 'audioStims/elonmusk.wav'},
    {'name': 'imgPerception/Slide148.png', 'path': 'imgPerception/Slide148.png'},
    {'name': 'imgPerception/Slide149.png', 'path': 'imgPerception/Slide149.png'},
    {'name': 'imgPerception/Slide150.png', 'path': 'imgPerception/Slide150.png'},
    {'name': 'audioStims/sarahjessicaparker.wav', 'path': 'audioStims/sarahjessicaparker.wav'},
    {'name': 'audioStims/barackobama.wav', 'path': 'audioStims/barackobama.wav'},
    {'name': 'imgPerception/Slide151.png', 'path': 'imgPerception/Slide151.png'},
    {'name': 'imgPerception/Slide152.png', 'path': 'imgPerception/Slide152.png'},
    {'name': 'imgPerception/Slide153.png', 'path': 'imgPerception/Slide153.png'},
    {'name': 'audioStims/princewilliam.wav', 'path': 'audioStims/princewilliam.wav'},
    {'name': 'audioStims/berniesanders.wav', 'path': 'audioStims/berniesanders.wav'},
    {'name': 'imgPerception/Slide154.png', 'path': 'imgPerception/Slide154.png'},
    {'name': 'imgPerception/Slide155.png', 'path': 'imgPerception/Slide155.png'},
    {'name': 'imgPerception/Slide156.png', 'path': 'imgPerception/Slide156.png'},
    {'name': 'audioStims/justinbieber.wav', 'path': 'audioStims/justinbieber.wav'},
    {'name': 'audioStims/johnnydepp.wav', 'path': 'audioStims/johnnydepp.wav'},
    {'name': 'imgPerception/Slide157.png', 'path': 'imgPerception/Slide157.png'},
    {'name': 'imgPerception/Slide158.png', 'path': 'imgPerception/Slide158.png'},
    {'name': 'imgPerception/Slide159.png', 'path': 'imgPerception/Slide159.png'},
    {'name': 'audioStims/georgewbush.wav', 'path': 'audioStims/georgewbush.wav'},
    {'name': 'audioStims/serenawilliams.wav', 'path': 'audioStims/serenawilliams.wav'},
    {'name': 'imgPerception/Slide160.png', 'path': 'imgPerception/Slide160.png'},
    {'name': 'imgPerception/Slide161.png', 'path': 'imgPerception/Slide161.png'},
    {'name': 'imgPerception/Slide162.png', 'path': 'imgPerception/Slide162.png'},
    {'name': 'audioStims/rihanna.wav', 'path': 'audioStims/rihanna.wav'},
    {'name': 'audioStims/bradpitt.wav', 'path': 'audioStims/bradpitt.wav'},
    {'name': 'imgPerception/Slide163.png', 'path': 'imgPerception/Slide163.png'},
    {'name': 'imgPerception/Slide164.png', 'path': 'imgPerception/Slide164.png'},
    {'name': 'imgPerception/Slide165.png', 'path': 'imgPerception/Slide165.png'},
    {'name': 'audioStims/stevejobs.wav', 'path': 'audioStims/stevejobs.wav'},
    {'name': 'audioStims/selenagomez.wav', 'path': 'audioStims/selenagomez.wav'},
    {'name': 'imgPerception/Slide166.png', 'path': 'imgPerception/Slide166.png'},
    {'name': 'imgPerception/Slide167.png', 'path': 'imgPerception/Slide167.png'},
    {'name': 'imgPerception/Slide168.png', 'path': 'imgPerception/Slide168.png'},
    {'name': 'audioStims/shakira.wav', 'path': 'audioStims/shakira.wav'},
    {'name': 'audioStims/markzuckerberg.wav', 'path': 'audioStims/markzuckerberg.wav'},
    {'name': 'imgPerception/Slide169.png', 'path': 'imgPerception/Slide169.png'},
    {'name': 'imgPerception/Slide170.png', 'path': 'imgPerception/Slide170.png'},
    {'name': 'imgPerception/Slide171.png', 'path': 'imgPerception/Slide171.png'},
    {'name': 'audioStims/michaelphelps.wav', 'path': 'audioStims/michaelphelps.wav'},
    {'name': 'imgPerception/Slide172.png', 'path': 'imgPerception/Slide172.png'},
    {'name': 'imgPerception/Slide173.png', 'path': 'imgPerception/Slide173.png'},
    {'name': 'imgPerception/Slide174.png', 'path': 'imgPerception/Slide174.png'},
    {'name': 'audioStims/oprahwinfrey.wav', 'path': 'audioStims/oprahwinfrey.wav'},
    {'name': 'imgPerception/Slide175.png', 'path': 'imgPerception/Slide175.png'},
    {'name': 'imgPerception/Slide176.png', 'path': 'imgPerception/Slide176.png'},
    {'name': 'imgPerception/Slide177.png', 'path': 'imgPerception/Slide177.png'},
    {'name': 'audioStims/taylorswift.wav', 'path': 'audioStims/taylorswift.wav'},
    {'name': 'audioStims/leonardodicaprio.wav', 'path': 'audioStims/leonardodicaprio.wav'},
    {'name': 'imgPerception/Slide178.png', 'path': 'imgPerception/Slide178.png'},
    {'name': 'imgPerception/Slide179.png', 'path': 'imgPerception/Slide179.png'},
    {'name': 'imgPerception/Slide180.png', 'path': 'imgPerception/Slide180.png'},
    {'name': 'SemanticsPractice_en.xlsx', 'path': 'SemanticsPractice_en.xlsx'},
    {'name': 'audioStims/brilliance.wav', 'path': 'audioStims/brilliance.wav'},
    {'name': 'audioStims/decision.wav', 'path': 'audioStims/decision.wav'},
    {'name': 'audioStims/merit.wav', 'path': 'audioStims/merit.wav'},
    {'name': 'SemanticItems.xlsx', 'path': 'SemanticItems.xlsx'},
    {'name': 'audioStims/Semantic.wav', 'path': 'audioStims/Semantic.wav'},
    {'name': 'audioStims/country.wav', 'path': 'audioStims/country.wav'},
    {'name': 'audioStims/philosophy.wav', 'path': 'audioStims/philosophy.wav'},
    {'name': 'audioStims/discipline.wav', 'path': 'audioStims/discipline.wav'},
    {'name': 'audioStims/health.wav', 'path': 'audioStims/health.wav'},
    {'name': 'audioStims/penalty.wav', 'path': 'audioStims/penalty.wav'},
    {'name': 'audioStims/comfort.wav', 'path': 'audioStims/comfort.wav'},
    {'name': 'audioStims/fidelity.wav', 'path': 'audioStims/fidelity.wav'},
    {'name': 'audioStims/generality.wav', 'path': 'audioStims/generality.wav'},
    {'name': 'audioStims/quality.wav', 'path': 'audioStims/quality.wav'},
    {'name': 'audioStims/motivation.wav', 'path': 'audioStims/motivation.wav'},
    {'name': 'audioStims/laziness.wav', 'path': 'audioStims/laziness.wav'},
    {'name': 'audioStims/anxiety.wav', 'path': 'audioStims/anxiety.wav'},
    {'name': 'audioStims/altruism.wav', 'path': 'audioStims/altruism.wav'},
    {'name': 'audioStims/prejudice.wav', 'path': 'audioStims/prejudice.wav'},
    {'name': 'audioStims/generosity.wav', 'path': 'audioStims/generosity.wav'},
    {'name': 'audioStims/discovery.wav', 'path': 'audioStims/discovery.wav'},
    {'name': 'audioStims/learning.wav', 'path': 'audioStims/learning.wav'},
    {'name': 'audioStims/mystery.wav', 'path': 'audioStims/mystery.wav'},
    {'name': 'audioStims/discussion.wav', 'path': 'audioStims/discussion.wav'},
    {'name': 'audioStims/reasoning.wav', 'path': 'audioStims/reasoning.wav'},
    {'name': 'audioStims/inference.wav', 'path': 'audioStims/inference.wav'},
    {'name': 'audioStims/reality.wav', 'path': 'audioStims/reality.wav'},
    {'name': 'audioStims/friendship.wav', 'path': 'audioStims/friendship.wav'},
    {'name': 'audioStims/illusion.wav', 'path': 'audioStims/illusion.wav'},
    {'name': 'audioStims/care.wav', 'path': 'audioStims/care.wav'},
    {'name': 'audioStims/access.wav', 'path': 'audioStims/access.wav'},
    {'name': 'audioStims/help.wav', 'path': 'audioStims/help.wav'},
    {'name': 'audioStims/mission.wav', 'path': 'audioStims/mission.wav'},
    {'name': 'audioStims/possession.wav', 'path': 'audioStims/possession.wav'},
    {'name': 'audioStims/employment.wav', 'path': 'audioStims/employment.wav'},
    {'name': 'audioStims/victory.wav', 'path': 'audioStims/victory.wav'},
    {'name': 'audioStims/future.wav', 'path': 'audioStims/future.wav'},
    {'name': 'audioStims/hope.wav', 'path': 'audioStims/hope.wav'},
    {'name': 'audioStims/routine.wav', 'path': 'audioStims/routine.wav'},
    {'name': 'audioStims/convention.wav', 'path': 'audioStims/convention.wav'},
    {'name': 'audioStims/society.wav', 'path': 'audioStims/society.wav'},
    {'name': 'audioStims/mariotest2.wav', 'path': 'audioStims/mariotest2.wav'},
    {'name': 'img/clavier.jpg', 'path': 'img/clavier.jpg'},
    {'name': 'img/vivacite.jpg', 'path': 'img/vivacite.jpg'},
    {'name': 'default.png', 'path': 'https://pavlovia.org/assets/default/default.png'},
    {'name': 'imgPerception/Slide1.png', 'path': 'imgPerception/Slide1.png'},
    {'name': 'imgPerception/Slide2.png', 'path': 'imgPerception/Slide2.png'},
    {'name': 'imgPerception/Slide3.png', 'path': 'imgPerception/Slide3.png'},
    {'name': 'imgPerception/Slide4.png', 'path': 'imgPerception/Slide4.png'},
    {'name': 'imgPerception/Slide5.png', 'path': 'imgPerception/Slide5.png'},
    {'name': 'imgPerception/Slide6.png', 'path': 'imgPerception/Slide6.png'},
    {'name': 'imgPerception/Slide7.png', 'path': 'imgPerception/Slide7.png'},
    {'name': 'imgPerception/Slide8.png', 'path': 'imgPerception/Slide8.png'},
    {'name': 'imgPerception/Slide9.png', 'path': 'imgPerception/Slide9.png'},
    {'name': 'imgPerception/Slide10.png', 'path': 'imgPerception/Slide10.png'},
    {'name': 'imgPerception/Slide11.png', 'path': 'imgPerception/Slide11.png'},
    {'name': 'imgPerception/Slide12.png', 'path': 'imgPerception/Slide12.png'},
    {'name': 'imgPerception/Slide13.png', 'path': 'imgPerception/Slide13.png'},
    {'name': 'imgPerception/Slide14.png', 'path': 'imgPerception/Slide14.png'},
    {'name': 'imgPerception/Slide15.png', 'path': 'imgPerception/Slide15.png'},
    {'name': 'imgPerception/Slide16.png', 'path': 'imgPerception/Slide16.png'},
    {'name': 'imgPerception/Slide17.png', 'path': 'imgPerception/Slide17.png'},
    {'name': 'imgPerception/Slide18.png', 'path': 'imgPerception/Slide18.png'},
    {'name': 'imgPerception/Slide19.png', 'path': 'imgPerception/Slide19.png'},
    {'name': 'imgPerception/Slide20.png', 'path': 'imgPerception/Slide20.png'},
    {'name': 'imgPerception/Slide21.png', 'path': 'imgPerception/Slide21.png'},
    {'name': 'imgPerception/Slide22.png', 'path': 'imgPerception/Slide22.png'},
    {'name': 'imgPerception/Slide23.png', 'path': 'imgPerception/Slide23.png'},
    {'name': 'imgPerception/Slide24.png', 'path': 'imgPerception/Slide24.png'},
    {'name': 'imgPerception/Slide25.png', 'path': 'imgPerception/Slide25.png'},
    {'name': 'imgPerception/Slide26.png', 'path': 'imgPerception/Slide26.png'},
    {'name': 'imgPerception/Slide27.png', 'path': 'imgPerception/Slide27.png'},
    {'name': 'imgPerception/Slide28.png', 'path': 'imgPerception/Slide28.png'},
    {'name': 'imgPerception/Slide29.png', 'path': 'imgPerception/Slide29.png'},
    {'name': 'imgPerception/Slide30.png', 'path': 'imgPerception/Slide30.png'},
    {'name': 'imgPerception/Slide31.png', 'path': 'imgPerception/Slide31.png'},
    {'name': 'imgPerception/Slide32.png', 'path': 'imgPerception/Slide32.png'},
    {'name': 'imgPerception/Slide33.png', 'path': 'imgPerception/Slide33.png'},
    {'name': 'imgPerception/Slide34.png', 'path': 'imgPerception/Slide34.png'},
    {'name': 'imgPerception/Slide35.png', 'path': 'imgPerception/Slide35.png'},
    {'name': 'imgPerception/Slide36.png', 'path': 'imgPerception/Slide36.png'},
    {'name': 'imgPerception/Slide37.png', 'path': 'imgPerception/Slide37.png'},
    {'name': 'imgPerception/Slide38.png', 'path': 'imgPerception/Slide38.png'},
    {'name': 'imgPerception/Slide39.png', 'path': 'imgPerception/Slide39.png'},
    {'name': 'imgPerception/Slide40.png', 'path': 'imgPerception/Slide40.png'},
    {'name': 'imgPerception/Slide41.png', 'path': 'imgPerception/Slide41.png'},
    {'name': 'imgPerception/Slide42.png', 'path': 'imgPerception/Slide42.png'},
    {'name': 'imgPerception/Slide43.png', 'path': 'imgPerception/Slide43.png'},
    {'name': 'imgPerception/Slide44.png', 'path': 'imgPerception/Slide44.png'},
    {'name': 'imgPerception/Slide45.png', 'path': 'imgPerception/Slide45.png'},
    {'name': 'imgPerception/Slide46.png', 'path': 'imgPerception/Slide46.png'},
    {'name': 'imgPerception/Slide47.png', 'path': 'imgPerception/Slide47.png'},
    {'name': 'imgPerception/Slide48.png', 'path': 'imgPerception/Slide48.png'},
    {'name': 'imgPerception/Slide49.png', 'path': 'imgPerception/Slide49.png'},
    {'name': 'imgPerception/Slide50.png', 'path': 'imgPerception/Slide50.png'},
    {'name': 'imgPerception/Slide51.png', 'path': 'imgPerception/Slide51.png'},
    {'name': 'imgPerception/Slide52.png', 'path': 'imgPerception/Slide52.png'},
    {'name': 'imgPerception/Slide53.png', 'path': 'imgPerception/Slide53.png'},
    {'name': 'imgPerception/Slide54.png', 'path': 'imgPerception/Slide54.png'},
    {'name': 'imgPerception/Slide55.png', 'path': 'imgPerception/Slide55.png'},
    {'name': 'imgPerception/Slide56.png', 'path': 'imgPerception/Slide56.png'},
    {'name': 'imgPerception/Slide57.png', 'path': 'imgPerception/Slide57.png'},
    {'name': 'imgPerception/Slide58.png', 'path': 'imgPerception/Slide58.png'},
    {'name': 'imgPerception/Slide59.png', 'path': 'imgPerception/Slide59.png'},
    {'name': 'imgPerception/Slide60.png', 'path': 'imgPerception/Slide60.png'},
    {'name': 'imgPerception/Slide61.png', 'path': 'imgPerception/Slide61.png'},
    {'name': 'imgPerception/Slide62.png', 'path': 'imgPerception/Slide62.png'},
    {'name': 'imgPerception/Slide63.png', 'path': 'imgPerception/Slide63.png'},
    {'name': 'imgPerception/Slide64.png', 'path': 'imgPerception/Slide64.png'},
    {'name': 'imgPerception/Slide65.png', 'path': 'imgPerception/Slide65.png'},
    {'name': 'imgPerception/Slide66.png', 'path': 'imgPerception/Slide66.png'},
    {'name': 'imgPerception/Slide67.png', 'path': 'imgPerception/Slide67.png'},
    {'name': 'imgPerception/Slide68.png', 'path': 'imgPerception/Slide68.png'},
    {'name': 'imgPerception/Slide69.png', 'path': 'imgPerception/Slide69.png'},
    {'name': 'imgPerception/Slide70.png', 'path': 'imgPerception/Slide70.png'},
    {'name': 'imgPerception/Slide71.png', 'path': 'imgPerception/Slide71.png'},
    {'name': 'imgPerception/Slide72.png', 'path': 'imgPerception/Slide72.png'},
    {'name': 'imgPerception/Slide73.png', 'path': 'imgPerception/Slide73.png'},
    {'name': 'imgPerception/Slide74.png', 'path': 'imgPerception/Slide74.png'},
    {'name': 'imgPerception/Slide75.png', 'path': 'imgPerception/Slide75.png'},
    {'name': 'imgPerception/Slide76.png', 'path': 'imgPerception/Slide76.png'},
    {'name': 'imgPerception/Slide77.png', 'path': 'imgPerception/Slide77.png'},
    {'name': 'imgPerception/Slide78.png', 'path': 'imgPerception/Slide78.png'},
    {'name': 'imgPerception/Slide79.png', 'path': 'imgPerception/Slide79.png'},
    {'name': 'imgPerception/Slide80.png', 'path': 'imgPerception/Slide80.png'},
    {'name': 'imgPerception/Slide81.png', 'path': 'imgPerception/Slide81.png'},
    {'name': 'imgPerception/Slide82.png', 'path': 'imgPerception/Slide82.png'},
    {'name': 'imgPerception/Slide83.png', 'path': 'imgPerception/Slide83.png'},
    {'name': 'imgPerception/Slide84.png', 'path': 'imgPerception/Slide84.png'},
    {'name': 'imgPerception/Slide85.png', 'path': 'imgPerception/Slide85.png'},
    {'name': 'imgPerception/Slide86.png', 'path': 'imgPerception/Slide86.png'},
    {'name': 'imgPerception/Slide87.png', 'path': 'imgPerception/Slide87.png'},
    {'name': 'imgPerception/Slide88.png', 'path': 'imgPerception/Slide88.png'},
    {'name': 'imgPerception/Slide89.png', 'path': 'imgPerception/Slide89.png'},
    {'name': 'imgPerception/Slide90.png', 'path': 'imgPerception/Slide90.png'},
    {'name': 'imgPerception/Slide91.png', 'path': 'imgPerception/Slide91.png'},
    {'name': 'imgPerception/Slide92.png', 'path': 'imgPerception/Slide92.png'},
    {'name': 'imgPerception/Slide93.png', 'path': 'imgPerception/Slide93.png'},
    {'name': 'imgPerception/Slide94.png', 'path': 'imgPerception/Slide94.png'},
    {'name': 'imgPerception/Slide95.png', 'path': 'imgPerception/Slide95.png'},
    {'name': 'imgPerception/Slide96.png', 'path': 'imgPerception/Slide96.png'},
    {'name': 'imgPerception/Slide97.png', 'path': 'imgPerception/Slide97.png'},
    {'name': 'imgPerception/Slide98.png', 'path': 'imgPerception/Slide98.png'},
    {'name': 'imgPerception/Slide99.png', 'path': 'imgPerception/Slide99.png'},
    {'name': 'imgPerception/Slide100.png', 'path': 'imgPerception/Slide100.png'},
    {'name': 'imgPerception/Slide101.png', 'path': 'imgPerception/Slide101.png'},
    {'name': 'imgPerception/Slide102.png', 'path': 'imgPerception/Slide102.png'},
    {'name': 'imgPerception/Slide103.png', 'path': 'imgPerception/Slide103.png'},
    {'name': 'imgPerception/Slide104.png', 'path': 'imgPerception/Slide104.png'},
    {'name': 'imgPerception/Slide105.png', 'path': 'imgPerception/Slide105.png'},
    {'name': 'imgPerception/Slide106.png', 'path': 'imgPerception/Slide106.png'},
    {'name': 'imgPerception/Slide107.png', 'path': 'imgPerception/Slide107.png'},
    {'name': 'imgPerception/Slide108.png', 'path': 'imgPerception/Slide108.png'},
    {'name': 'imgPerception/Slide109.png', 'path': 'imgPerception/Slide109.png'},
    {'name': 'imgPerception/Slide110.png', 'path': 'imgPerception/Slide110.png'},
    {'name': 'imgPerception/Slide111.png', 'path': 'imgPerception/Slide111.png'},
    {'name': 'imgPerception/Slide112.png', 'path': 'imgPerception/Slide112.png'},
    {'name': 'imgPerception/Slide113.png', 'path': 'imgPerception/Slide113.png'},
    {'name': 'imgPerception/Slide114.png', 'path': 'imgPerception/Slide114.png'},
    {'name': 'imgPerception/Slide115.png', 'path': 'imgPerception/Slide115.png'},
    {'name': 'imgPerception/Slide116.png', 'path': 'imgPerception/Slide116.png'},
    {'name': 'imgPerception/Slide117.png', 'path': 'imgPerception/Slide117.png'},
    {'name': 'imgPerception/Slide118.png', 'path': 'imgPerception/Slide118.png'},
    {'name': 'imgPerception/Slide119.png', 'path': 'imgPerception/Slide119.png'},
    {'name': 'imgPerception/Slide120.png', 'path': 'imgPerception/Slide120.png'},
    {'name': 'imgPerception/Slide121.png', 'path': 'imgPerception/Slide121.png'},
    {'name': 'imgPerception/Slide122.png', 'path': 'imgPerception/Slide122.png'},
    {'name': 'imgPerception/Slide123.png', 'path': 'imgPerception/Slide123.png'},
    {'name': 'imgPerception/Slide124.png', 'path': 'imgPerception/Slide124.png'},
    {'name': 'imgPerception/Slide125.png', 'path': 'imgPerception/Slide125.png'},
    {'name': 'imgPerception/Slide126.png', 'path': 'imgPerception/Slide126.png'},
    {'name': 'imgPerception/Slide127.png', 'path': 'imgPerception/Slide127.png'},
    {'name': 'imgPerception/Slide128.png', 'path': 'imgPerception/Slide128.png'},
    {'name': 'imgPerception/Slide129.png', 'path': 'imgPerception/Slide129.png'},
    {'name': 'imgPerception/Slide130.png', 'path': 'imgPerception/Slide130.png'},
    {'name': 'imgPerception/Slide131.png', 'path': 'imgPerception/Slide131.png'},
    {'name': 'imgPerception/Slide132.png', 'path': 'imgPerception/Slide132.png'},
    {'name': 'imgPerception/Slide133.png', 'path': 'imgPerception/Slide133.png'},
    {'name': 'imgPerception/Slide134.png', 'path': 'imgPerception/Slide134.png'},
    {'name': 'imgPerception/Slide135.png', 'path': 'imgPerception/Slide135.png'},
    {'name': 'imgPerception/Slide136.png', 'path': 'imgPerception/Slide136.png'},
    {'name': 'imgPerception/Slide137.png', 'path': 'imgPerception/Slide137.png'},
    {'name': 'imgPerception/Slide138.png', 'path': 'imgPerception/Slide138.png'},
    {'name': 'imgPerception/Slide139.png', 'path': 'imgPerception/Slide139.png'},
    {'name': 'imgPerception/Slide140.png', 'path': 'imgPerception/Slide140.png'},
    {'name': 'imgPerception/Slide141.png', 'path': 'imgPerception/Slide141.png'},
    {'name': 'imgPerception/Slide142.png', 'path': 'imgPerception/Slide142.png'},
    {'name': 'imgPerception/Slide143.png', 'path': 'imgPerception/Slide143.png'},
    {'name': 'imgPerception/Slide144.png', 'path': 'imgPerception/Slide144.png'},
    {'name': 'imgPerception/Slide145.png', 'path': 'imgPerception/Slide145.png'},
    {'name': 'imgPerception/Slide146.png', 'path': 'imgPerception/Slide146.png'},
    {'name': 'imgPerception/Slide147.png', 'path': 'imgPerception/Slide147.png'},
    {'name': 'imgPerception/Slide148.png', 'path': 'imgPerception/Slide148.png'},
    {'name': 'imgPerception/Slide149.png', 'path': 'imgPerception/Slide149.png'},
    {'name': 'imgPerception/Slide150.png', 'path': 'imgPerception/Slide150.png'},
    {'name': 'imgPerception/Slide151.png', 'path': 'imgPerception/Slide151.png'},
    {'name': 'imgPerception/Slide152.png', 'path': 'imgPerception/Slide152.png'},
    {'name': 'imgPerception/Slide153.png', 'path': 'imgPerception/Slide153.png'},
    {'name': 'imgPerception/Slide154.png', 'path': 'imgPerception/Slide154.png'},
    {'name': 'imgPerception/Slide155.png', 'path': 'imgPerception/Slide155.png'},
    {'name': 'imgPerception/Slide156.png', 'path': 'imgPerception/Slide156.png'},
    {'name': 'imgPerception/Slide157.png', 'path': 'imgPerception/Slide157.png'},
    {'name': 'imgPerception/Slide158.png', 'path': 'imgPerception/Slide158.png'},
    {'name': 'imgPerception/Slide159.png', 'path': 'imgPerception/Slide159.png'},
    {'name': 'imgPerception/Slide160.png', 'path': 'imgPerception/Slide160.png'},
    {'name': 'imgPerception/Slide161.png', 'path': 'imgPerception/Slide161.png'},
    {'name': 'imgPerception/Slide162.png', 'path': 'imgPerception/Slide162.png'},
    {'name': 'imgPerception/Slide163.png', 'path': 'imgPerception/Slide163.png'},
    {'name': 'imgPerception/Slide164.png', 'path': 'imgPerception/Slide164.png'},
    {'name': 'imgPerception/Slide165.png', 'path': 'imgPerception/Slide165.png'},
    {'name': 'imgPerception/Slide166.png', 'path': 'imgPerception/Slide166.png'},
    {'name': 'imgPerception/Slide167.png', 'path': 'imgPerception/Slide167.png'},
    {'name': 'imgPerception/Slide168.png', 'path': 'imgPerception/Slide168.png'},
    {'name': 'imgPerception/Slide169.png', 'path': 'imgPerception/Slide169.png'},
    {'name': 'imgPerception/Slide170.png', 'path': 'imgPerception/Slide170.png'},
    {'name': 'imgPerception/Slide171.png', 'path': 'imgPerception/Slide171.png'},
    {'name': 'imgPerception/Slide172.png', 'path': 'imgPerception/Slide172.png'},
    {'name': 'imgPerception/Slide173.png', 'path': 'imgPerception/Slide173.png'},
    {'name': 'imgPerception/Slide174.png', 'path': 'imgPerception/Slide174.png'},
    {'name': 'imgPerception/Slide175.png', 'path': 'imgPerception/Slide175.png'},
    {'name': 'imgPerception/Slide176.png', 'path': 'imgPerception/Slide176.png'},
    {'name': 'imgPerception/Slide177.png', 'path': 'imgPerception/Slide177.png'},
    {'name': 'imgPerception/Slide178.png', 'path': 'imgPerception/Slide178.png'},
    {'name': 'imgPerception/Slide179.png', 'path': 'imgPerception/Slide179.png'},
    {'name': 'imgPerception/Slide180.png', 'path': 'imgPerception/Slide180.png'},
    {'name': 'imgPerception/Slide181.png', 'path': 'imgPerception/Slide181.png'},
    {'name': 'imgPerception/Slide182.png', 'path': 'imgPerception/Slide182.png'},
    {'name': 'imgPerception/Slide183.png', 'path': 'imgPerception/Slide183.png'},
    {'name': 'imgPerception/Slide184.png', 'path': 'imgPerception/Slide184.png'},
    {'name': 'imgPerception/Slide185.png', 'path': 'imgPerception/Slide185.png'},
    {'name': 'img/clavier.jpg', 'path': 'img/clavier.jpg'},
    {'name': 'img/Colour_Trial.png', 'path': 'img/Colour_Trial.png'},
    {'name': 'img/Face_Trial.png', 'path': 'img/Face_Trial.png'},
    {'name': 'img/Letter_Trial.png', 'path': 'img/Letter_Trial.png'},
    {'name': 'img/Map_Trial.png', 'path': 'img/Map_Trial.png'},
    {'name': 'img/Shape_Trial.png', 'path': 'img/Shape_Trial.png'},
    {'name': 'img/vivacite.jpg', 'path': 'img/vivacite.jpg'},
    {'name': 'audioStims/access.wav', 'path': 'audioStims/access.wav'},
    {'name': 'audioStims/altruism.wav', 'path': 'audioStims/altruism.wav'},
    {'name': 'audioStims/anxiety.wav', 'path': 'audioStims/anxiety.wav'},
    {'name': 'audioStims/artichokes.wav', 'path': 'audioStims/artichokes.wav'},
    {'name': 'audioStims/atlanta.wav', 'path': 'audioStims/atlanta.wav'},
    {'name': 'audioStims/atlanticcity.wav', 'path': 'audioStims/atlanticcity.wav'},
    {'name': 'audioStims/austin.wav', 'path': 'audioStims/austin.wav'},
    {'name': 'audioStims/bad.wav', 'path': 'audioStims/bad.wav'},
    {'name': 'audioStims/baguette.wav', 'path': 'audioStims/baguette.wav'},
    {'name': 'audioStims/baltimore.wav', 'path': 'audioStims/baltimore.wav'},
    {'name': 'audioStims/banana.wav', 'path': 'audioStims/banana.wav'},
    {'name': 'audioStims/barackobama.wav', 'path': 'audioStims/barackobama.wav'},
    {'name': 'audioStims/bathtub.wav', 'path': 'audioStims/bathtub.wav'},
    {'name': 'audioStims/berniesanders.wav', 'path': 'audioStims/berniesanders.wav'},
    {'name': 'audioStims/boston.wav', 'path': 'audioStims/boston.wav'},
    {'name': 'audioStims/bottle.wav', 'path': 'audioStims/bottle.wav'},
    {'name': 'audioStims/bowl.wav', 'path': 'audioStims/bowl.wav'},
    {'name': 'audioStims/bradpitt.wav', 'path': 'audioStims/bradpitt.wav'},
    {'name': 'audioStims/brilliance.wav', 'path': 'audioStims/brilliance.wav'},
    {'name': 'audioStims/broom.wav', 'path': 'audioStims/broom.wav'},
    {'name': 'audioStims/bucket.wav', 'path': 'audioStims/bucket.wav'},
    {'name': 'audioStims/burrito.wav', 'path': 'audioStims/burrito.wav'},
    {'name': 'audioStims/care.wav', 'path': 'audioStims/care.wav'},
    {'name': 'audioStims/cauliflower.wav', 'path': 'audioStims/cauliflower.wav'},
    {'name': 'audioStims/chicken.wav', 'path': 'audioStims/chicken.wav'},
    {'name': 'audioStims/cincinnati.wav', 'path': 'audioStims/cincinnati.wav'},
    {'name': 'audioStims/cold.wav', 'path': 'audioStims/cold.wav'},
    {'name': 'audioStims/Color.wav', 'path': 'audioStims/Color.wav'},
    {'name': 'audioStims/comfort.wav', 'path': 'audioStims/comfort.wav'},
    {'name': 'audioStims/convention.wav', 'path': 'audioStims/convention.wav'},
    {'name': 'audioStims/country.wav', 'path': 'audioStims/country.wav'},
    {'name': 'audioStims/cucumber.wav', 'path': 'audioStims/cucumber.wav'},
    {'name': 'audioStims/culture.wav', 'path': 'audioStims/culture.wav'},
    {'name': 'audioStims/dallas.wav', 'path': 'audioStims/dallas.wav'},
    {'name': 'audioStims/dark.wav', 'path': 'audioStims/dark.wav'},
    {'name': 'audioStims/decision.wav', 'path': 'audioStims/decision.wav'},
    {'name': 'audioStims/detroit.wav', 'path': 'audioStims/detroit.wav'},
    {'name': 'audioStims/discipline.wav', 'path': 'audioStims/discipline.wav'},
    {'name': 'audioStims/discovery.wav', 'path': 'audioStims/discovery.wav'},
    {'name': 'audioStims/discussion.wav', 'path': 'audioStims/discussion.wav'},
    {'name': 'audioStims/down.wav', 'path': 'audioStims/down.wav'},
    {'name': 'audioStims/elonmusk.wav', 'path': 'audioStims/elonmusk.wav'},
    {'name': 'audioStims/emmastone.wav', 'path': 'audioStims/emmastone.wav'},
    {'name': 'audioStims/employment.wav', 'path': 'audioStims/employment.wav'},
    {'name': 'audioStims/espresso.wav', 'path': 'audioStims/espresso.wav'},
    {'name': 'audioStims/evocation.wav', 'path': 'audioStims/evocation.wav'},
    {'name': 'audioStims/Face.wav', 'path': 'audioStims/Face.wav'},
    {'name': 'audioStims/fidelity.wav', 'path': 'audioStims/fidelity.wav'},
    {'name': 'audioStims/fig.wav', 'path': 'audioStims/fig.wav'},
    {'name': 'audioStims/flamingo.wav', 'path': 'audioStims/flamingo.wav'},
    {'name': 'audioStims/friendship.wav', 'path': 'audioStims/friendship.wav'},
    {'name': 'audioStims/future.wav', 'path': 'audioStims/future.wav'},
    {'name': 'audioStims/generality.wav', 'path': 'audioStims/generality.wav'},
    {'name': 'audioStims/generosity.wav', 'path': 'audioStims/generosity.wav'},
    {'name': 'audioStims/georgewbush.wav', 'path': 'audioStims/georgewbush.wav'},
    {'name': 'audioStims/greenapple.wav', 'path': 'audioStims/greenapple.wav'},
    {'name': 'audioStims/greenpepper.wav', 'path': 'audioStims/greenpepper.wav'},
    {'name': 'audioStims/guacamole.wav', 'path': 'audioStims/guacamole.wav'},
    {'name': 'audioStims/hamster.wav', 'path': 'audioStims/hamster.wav'},
    {'name': 'audioStims/health.wav', 'path': 'audioStims/health.wav'},
    {'name': 'audioStims/help.wav', 'path': 'audioStims/help.wav'},
    {'name': 'audioStims/hope.wav', 'path': 'audioStims/hope.wav'},
    {'name': 'audioStims/houston.wav', 'path': 'audioStims/houston.wav'},
    {'name': 'audioStims/illusion.wav', 'path': 'audioStims/illusion.wav'},
    {'name': 'audioStims/impression.wav', 'path': 'audioStims/impression.wav'},
    {'name': 'audioStims/inference.wav', 'path': 'audioStims/inference.wav'},
    {'name': 'audioStims/johnnydepp.wav', 'path': 'audioStims/johnnydepp.wav'},
    {'name': 'audioStims/juliaroberts.wav', 'path': 'audioStims/juliaroberts.wav'},
    {'name': 'audioStims/justice.wav', 'path': 'audioStims/justice.wav'},
    {'name': 'audioStims/justinbieber.wav', 'path': 'audioStims/justinbieber.wav'},
    {'name': 'audioStims/justintimberlake.wav', 'path': 'audioStims/justintimberlake.wav'},
    {'name': 'audioStims/kamalaharris.wav', 'path': 'audioStims/kamalaharris.wav'},
    {'name': 'audioStims/kangarou.wav', 'path': 'audioStims/kangarou.wav'},
    {'name': 'audioStims/kansascity.wav', 'path': 'audioStims/kansascity.wav'},
    {'name': 'audioStims/kevinhart.wav', 'path': 'audioStims/kevinhart.wav'},
    {'name': 'audioStims/koala.wav', 'path': 'audioStims/koala.wav'},
    {'name': 'audioStims/ladybug.wav', 'path': 'audioStims/ladybug.wav'},
    {'name': 'audioStims/lasvegas.wav', 'path': 'audioStims/lasvegas.wav'},
    {'name': 'audioStims/laziness.wav', 'path': 'audioStims/laziness.wav'},
    {'name': 'audioStims/learning.wav', 'path': 'audioStims/learning.wav'},
    {'name': 'audioStims/left.wav', 'path': 'audioStims/left.wav'},
    {'name': 'audioStims/lemon.wav', 'path': 'audioStims/lemon.wav'},
    {'name': 'audioStims/leonardodicaprio.wav', 'path': 'audioStims/leonardodicaprio.wav'},
    {'name': 'audioStims/Letter.wav', 'path': 'audioStims/Letter.wav'},
    {'name': 'audioStims/light.wav', 'path': 'audioStims/light.wav'},
    {'name': 'audioStims/lighter.wav', 'path': 'audioStims/lighter.wav'},
    {'name': 'audioStims/long.wav', 'path': 'audioStims/long.wav'},
    {'name': 'audioStims/Map.wav', 'path': 'audioStims/Map.wav'},
    {'name': 'audioStims/mariotest2.wav', 'path': 'audioStims/mariotest2.wav'},
    {'name': 'audioStims/markzuckerberg.wav', 'path': 'audioStims/markzuckerberg.wav'},
    {'name': 'audioStims/marshmellow.wav', 'path': 'audioStims/marshmellow.wav'},
    {'name': 'audioStims/matthewmcconaughey.wav', 'path': 'audioStims/matthewmcconaughey.wav'},
    {'name': 'audioStims/memphis.wav', 'path': 'audioStims/memphis.wav'},
    {'name': 'audioStims/mention.wav', 'path': 'audioStims/mention.wav'},
    {'name': 'audioStims/merit.wav', 'path': 'audioStims/merit.wav'},
    {'name': 'audioStims/miami.wav', 'path': 'audioStims/miami.wav'},
    {'name': 'audioStims/michaelphelps.wav', 'path': 'audioStims/michaelphelps.wav'},
    {'name': 'audioStims/minneapolis.wav', 'path': 'audioStims/minneapolis.wav'},
    {'name': 'audioStims/mission.wav', 'path': 'audioStims/mission.wav'},
    {'name': 'audioStims/motivation.wav', 'path': 'audioStims/motivation.wav'},
    {'name': 'audioStims/mystery.wav', 'path': 'audioStims/mystery.wav'},
    {'name': 'audioStims/nashville.wav', 'path': 'audioStims/nashville.wav'},
    {'name': 'audioStims/noise.wav', 'path': 'audioStims/noise.wav'},
    {'name': 'audioStims/opinion.wav', 'path': 'audioStims/opinion.wav'},
    {'name': 'audioStims/oprahwinfrey.wav', 'path': 'audioStims/oprahwinfrey.wav'},
    {'name': 'audioStims/pan.wav', 'path': 'audioStims/pan.wav'},
    {'name': 'audioStims/panda.wav', 'path': 'audioStims/panda.wav'},
    {'name': 'audioStims/penalty.wav', 'path': 'audioStims/penalty.wav'},
    {'name': 'audioStims/penguin.wav', 'path': 'audioStims/penguin.wav'},
    {'name': 'audioStims/philadelphia.wav', 'path': 'audioStims/philadelphia.wav'},
    {'name': 'audioStims/philosophy.wav', 'path': 'audioStims/philosophy.wav'},
    {'name': 'audioStims/phoenix.wav', 'path': 'audioStims/phoenix.wav'},
    {'name': 'audioStims/pineapple.wav', 'path': 'audioStims/pineapple.wav'},
    {'name': 'audioStims/pomogranate.wav', 'path': 'audioStims/pomogranate.wav'},
    {'name': 'audioStims/portland.wav', 'path': 'audioStims/portland.wav'},
    {'name': 'audioStims/possession.wav', 'path': 'audioStims/possession.wav'},
    {'name': 'audioStims/prejudice.wav', 'path': 'audioStims/prejudice.wav'},
    {'name': 'audioStims/princewilliam.wav', 'path': 'audioStims/princewilliam.wav'},
    {'name': 'audioStims/prove.wav', 'path': 'audioStims/prove.wav'},
    {'name': 'audioStims/pumpkin.wav', 'path': 'audioStims/pumpkin.wav'},
    {'name': 'audioStims/quality.wav', 'path': 'audioStims/quality.wav'},
    {'name': 'audioStims/reality.wav', 'path': 'audioStims/reality.wav'},
    {'name': 'audioStims/reasoning.wav', 'path': 'audioStims/reasoning.wav'},
    {'name': 'audioStims/redwine.wav', 'path': 'audioStims/redwine.wav'},
    {'name': 'audioStims/reject.wav', 'path': 'audioStims/reject.wav'},
    {'name': 'audioStims/retrieve.wav', 'path': 'audioStims/retrieve.wav'},
    {'name': 'audioStims/right.wav', 'path': 'audioStims/right.wav'},
    {'name': 'audioStims/rigor.wav', 'path': 'audioStims/rigor.wav'},
    {'name': 'audioStims/rihanna.wav', 'path': 'audioStims/rihanna.wav'},
    {'name': 'audioStims/round.wav', 'path': 'audioStims/round.wav'},
    {'name': 'audioStims/routine.wav', 'path': 'audioStims/routine.wav'},
    {'name': 'audioStims/sarahjessicaparker.wav', 'path': 'audioStims/sarahjessicaparker.wav'},
    {'name': 'audioStims/seattle.wav', 'path': 'audioStims/seattle.wav'},
    {'name': 'audioStims/selenagomez.wav', 'path': 'audioStims/selenagomez.wav'},
    {'name': 'audioStims/Semantic.wav', 'path': 'audioStims/Semantic.wav'},
    {'name': 'audioStims/sentence.wav', 'path': 'audioStims/sentence.wav'},
    {'name': 'audioStims/serenawilliams.wav', 'path': 'audioStims/serenawilliams.wav'},
    {'name': 'audioStims/shakira.wav', 'path': 'audioStims/shakira.wav'},
    {'name': 'audioStims/Shape.wav', 'path': 'audioStims/Shape.wav'},
    {'name': 'audioStims/silence.wav', 'path': 'audioStims/silence.wav'},
    {'name': 'audioStims/smart.wav', 'path': 'audioStims/smart.wav'},
    {'name': 'audioStims/society.wav', 'path': 'audioStims/society.wav'},
    {'name': 'audioStims/spirit.wav', 'path': 'audioStims/spirit.wav'},
    {'name': 'audioStims/stevejobs.wav', 'path': 'audioStims/stevejobs.wav'},
    {'name': 'audioStims/strawberry.wav', 'path': 'audioStims/strawberry.wav'},
    {'name': 'audioStims/summary.wav', 'path': 'audioStims/summary.wav'},
    {'name': 'audioStims/taylorswift.wav', 'path': 'audioStims/taylorswift.wav'},
    {'name': 'audioStims/tripod.wav', 'path': 'audioStims/tripod.wav'},
    {'name': 'audioStims/up.wav', 'path': 'audioStims/up.wav'},
    {'name': 'audioStims/victory.wav', 'path': 'audioStims/victory.wav'},
    {'name': 'audioStims/warmth.wav', 'path': 'audioStims/warmth.wav'},
    {'name': 'audioStims/washingtondc.wav', 'path': 'audioStims/washingtondc.wav'},
    {'name': 'audioStims/whiteshark.wav', 'path': 'audioStims/whiteshark.wav'},
    {'name': 'audioStims/willsmith.wav', 'path': 'audioStims/willsmith.wav'},
    {'name': 'audioStims/year.wav', 'path': 'audioStims/year.wav'},
    {'name': 'audioStims/zucchini.wav', 'path': 'audioStims/zucchini.wav'},
  ]
});

psychoJS.experimentLogger.setLevel(core.Logger.ServerLevel.EXP);


var currentLoop;
var frameDur;
async function updateInfo() {
  currentLoop = psychoJS.experiment;  // right now there are no loops
  expInfo['date'] = util.MonotonicClock.getDateStr();  // add a simple timestamp
  expInfo['expName'] = expName;
  expInfo['psychopyVersion'] = '2024.2.4';
  expInfo['OS'] = window.navigator.platform;


  // store frame rate of monitor if we can measure it successfully
  expInfo['frameRate'] = psychoJS.window.getActualFrameRate();
  if (typeof expInfo['frameRate'] !== 'undefined')
    frameDur = 1.0 / Math.round(expInfo['frameRate']);
  else
    frameDur = 1.0 / 60.0; // couldn't get a reliable measure so guess

  // add info from the URL:
  util.addInfoFromUrl(expInfo);
  

  
  psychoJS.experiment.dataFileName = (("." + "/") + `data/${expInfo["participant"]}_${expName}_${expInfo["date"]}`);
  psychoJS.experiment.field_separator = '\t';


  return Scheduler.Event.NEXT;
}


var resource_manageClock;
var volume_testClock;
var volume_title;
var volume_response;
var volume_text;
var volume_continue;
var sound_1;
var key_testClock;
var key_test_title;
var key_test_text;
var key_test_display;
var key_test_show;
var key_test_continue;
var key_test_image;
var begin_instructionClock;
var begin_text;
var begin_response;
var begin_continue;
var vivid_instructionClock;
var vivid_title;
var vivid_response;
var vivid_mona_lisa;
var vivid_rating;
var vivid_image;
var vivid_text;
var vviq_instructionClock;
var vviq_instruction_title;
var vviq_instruction_text;
var vviq_instruction_proceed;
var vviq_instruction_response;
var vviq_instruction_good;
var vviq_taskClock;
var vviq_task_title;
var key_resp_vviq;
var vviq_task_score;
var vviq_task_idx;
var vviq_task_item;
var vviq_task_senario;
var vviq_feedbackClock;
var vviq_feedback_title;
var msg;
var colortext;
var previousLength;
var vviq_feedback_proceed;
var vviq_feedback_response;
var vviq_feedback_result;
var imagery_instructionClock;
var imagery_instruction_title;
var imagery_instruction_text;
var imagery_instruction_proceed;
var imagery_instruction_proceed_2;
var imagery_task_trialsClock;
var imagery_task_trials_question;
var imagery_task_trials_word1;
var imagery_task_trials_word2;
var imagery_task_trials_instruction;
var imagery_task_trials_response;
var imagery_task_trials_order_instruction;
var imagery_task_trials_img1;
var imagery_task_trials_proceed;
var imagery_task_trials_vividnessClock;
var imagery_task_trials_vividness_question;
var imagery_task_trials_vividness_text;
var imagery_task_trials_vividness_response;
var restingClock;
var imagery_task_trials_rest;
var imagery_task_prepareClock;
var imagery_task_prepare_title;
var imagery_task_prepare_text;
var imagery_task_prepare_response;
var imagery_task_prepare_proceed;
var imagery_taskClock;
var imagery_task_title;
var imagery_task_idx;
var imagery_task_word1;
var imagery_task_word2;
var imagery_task_instruction;
var imagery_task_text;
var key_resp_vmi;
var imagery_task_vividnessClock;
var imagery_task_vividness_title;
var vividness;
var imagery_task_feedbackClock;
var imagery_task_feedback_title;
var imagery_task_feedback_text_1;
var imagery_task_feedback_response;
var imagery_task_feedback_proceed;
var perception_instructionClock;
var perception_instruction_title;
var perception_instruction_task;
var perception_instruction_response;
var perception_instruction_proceed;
var perception_taskClock;
var perception_task_title;
var perception_task_word1;
var perception_task_img1;
var perception_task_word2;
var perception_task_img2;
var perception_task_instruction;
var perception_task_img3;
var perception;
var perception_task_text;
var perception_task_confidenceClock;
var perception_task_confidence_idx;
var perception_task_confidence_text;
var confidence;
var perception_task_feedbackClock;
var perception_task_feedback_title;
var perception_task_feedback_text_1;
var perception_task_feedback_response;
var perception_task_feedback_proceed;
var sem_instructionClock;
var semantics_instruction_title;
var semantics_instruction_task;
var semantics_instruction_response;
var semantics_instruction_proceed;
var semantics_task_trialsClock;
var semantics_task_trials_question;
var semantics_task_trials_word1;
var semantics_task_trials_word2;
var semantics_task_trials_instruction;
var semantics_task_trials_response;
var semantics_task_trials_order_instruction;
var semantics_task_trials_proceed;
var sem_trialClock;
var semantics_task_title;
var semanticBlock_idx;
var fixation;
var field_s;
var word1_s;
var word2_s;
var instr_s;
var key_resp_vmi_s;
var end_sessionClock;
var end_session_text;
var globalClock;
var routineTimer;
async function experimentInit() {
  // Initialize components for Routine "resource_manage"
  resource_manageClock = new util.Clock();
  // Initialize components for Routine "volume_test"
  volume_testClock = new util.Clock();
  volume_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'volume_title',
    text: 'Volume Calibration',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  volume_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  volume_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'volume_text',
    text: 'Make sure you can hear the music and that the volume is correct.\n\nWe recommend that you wear headphones for this test and turn the sound up to a comfortable level for you.',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -2.0 
  });
  
  volume_continue = new visual.TextStim({
    win: psychoJS.window,
    name: 'volume_continue',
    text: "Press the < Space > key to continue. \nIf you don't hear the music, press the <Esc> key.\n",
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.35)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -3.0 
  });
  
  sound_1 = new sound.Sound({
      win: psychoJS.window,
      value: 'audioStims/mariotest2.wav',
      secs: (- 1),
      });
  sound_1.setVolume(8);
  // Initialize components for Routine "key_test"
  key_testClock = new util.Clock();
  key_test_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'key_test_title',
    text: 'Keyboard Calibration',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('red'),  opacity: 1,
    depth: 0.0 
  });
  
  key_test_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'key_test_text',
    text: "During this test, you will have to use the following keys on the TOP LEFT part of your keyboard:\n              '1',  '2',  '3',  '4',  '5'\n\nPress the buttons one by one, make sure your keyboard works well.\n\nThe button you just pressed is:\n\n\n",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.1], draggable: false, height: 0.035,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  key_test_display = new visual.TextStim({
    win: psychoJS.window,
    name: 'key_test_display',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.05)], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -2.0 
  });
  
  key_test_show = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  key_test_continue = new visual.TextStim({
    win: psychoJS.window,
    name: 'key_test_continue',
    text: 'If your keyboard is working, press <Enter>.\n',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.5)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -5.0 
  });
  
  key_test_image = new visual.ImageStim({
    win : psychoJS.window,
    name : 'key_test_image', units : undefined, 
    image : 'img/clavier.jpg', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, (- 0.3)], 
    draggable: false,
    size : [0.65, 0.3],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -6.0 
  });
  // Initialize components for Routine "begin_instruction"
  begin_instructionClock = new util.Clock();
  begin_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'begin_text',
    text: "Let's start the first part of the test, are you ready?",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  begin_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  begin_continue = new visual.TextStim({
    win: psychoJS.window,
    name: 'begin_continue',
    text: 'Proceed by pressing < Space >.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  // Initialize components for Routine "vivid_instruction"
  vivid_instructionClock = new util.Clock();
  vivid_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'vivid_title',
    text: 'Part 1/4: Evaluation of Vividness of mental imagery  ',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  vivid_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  vivid_mona_lisa = new visual.TextStim({
    win: psychoJS.window,
    name: 'vivid_mona_lisa',
    text: 'For example, imagine the painting Mona Lisa.',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.33], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  vivid_rating = new visual.TextStim({
    win: psychoJS.window,
    name: 'vivid_rating',
    text: 'We will describe scenarios that you will have to imagine in your mind. Then we will ask you to give a score from 1 to 5 to evaluate its liveliness.\n\nAssess the vividness of your mental image with a score from 1 to 5:\n\n1: No image is visible.\n2: the image is vague and imprecise.\n3: The image is moderately clear and sharp.\n4: the image is relatively clear, almost as sharp and precise as a visual perception.\n5: the image is perfectly clear, as sharp and precise as real visual perception. ',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.1], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -3.0 
  });
  
  vivid_image = new visual.ImageStim({
    win : psychoJS.window,
    name : 'vivid_image', units : undefined, 
    image : 'img/vivacite.jpg', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, (- 0.28)], 
    draggable: false,
    size : [0.95, 0.2],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -4.0 
  });
  vivid_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'vivid_text',
    text: 'Enter a rating (1-5) to continue. Use the TOP LEFT part of your keyboard.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -5.0 
  });
  
  // Initialize components for Routine "vviq_instruction"
  vviq_instructionClock = new util.Clock();
  vviq_instruction_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_instruction_title',
    text: 'Part 1/4: Vividness Test',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  vviq_instruction_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_instruction_text',
    text: "We are going to describe some scenarios to you, which you need to imagine in your mind. \nNext, we'll ask you to rate it from 1 to 5 to assess its vividness\n\nThis task takes about 3 minutes.",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  vviq_instruction_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_instruction_proceed',
    text: 'Press the < Space > key to proceed.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  vviq_instruction_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  vviq_instruction_good = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_instruction_good',
    text: 'Good!',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.15], draggable: false, height: 0.05,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -4.0 
  });
  
  // Initialize components for Routine "vviq_task"
  vviq_taskClock = new util.Clock();
  vviq_task_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_task_title',
    text: 'Part 1/4: Test of Mental Imagery Vividness',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  key_resp_vviq = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  vviq_task_score = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_task_score',
    text: 'Give a score of 1 to 5 to evaluate the vividness of your visual mental imagery',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  vviq_task_idx = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_task_idx',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.25], draggable: false, height: 0.05,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -3.0 
  });
  
  vviq_task_item = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_task_item',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -4.0 
  });
  
  vviq_task_senario = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_task_senario',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.15], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -5.0 
  });
  
  // Initialize components for Routine "vviq_feedback"
  vviq_feedbackClock = new util.Clock();
  vviq_feedback_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_feedback_title',
    text: 'Part 1/4: Vividness test result',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  // Run 'Begin Experiment' code from vviq_feedback_text
  msg='doh';
  colortext = 'black';
  previousLength = 0;
  vviq_feedback_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_feedback_proceed',
    text: 'Press < Space > key to proceed to the second part.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.2)], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  vviq_feedback_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  vviq_feedback_result = new visual.TextStim({
    win: psychoJS.window,
    name: 'vviq_feedback_result',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('white'),  opacity: 1,
    depth: -4.0 
  });
  
  // Initialize components for Routine "imagery_instruction"
  imagery_instructionClock = new util.Clock();
  imagery_instruction_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_instruction_title',
    text: 'Part 2/4: Visual Mental Imagery Test',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  imagery_instruction_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_instruction_text',
    text: 'In this second part, we will evaluate your Visual Mental Imagery through tests covering five categories: shape, color, face, letter and map.\n\nFor each category, you will start with a trial. Then, you will need to respond to the following 15 tests.\n\nThis Visual Mental Imagery task will take about 10 minutes.',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  imagery_instruction_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_instruction_proceed',
    text: 'Press < Space > to proceed.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -2.0 
  });
  
  imagery_instruction_proceed_2 = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "imagery_task_trials"
  imagery_task_trialsClock = new util.Clock();
  imagery_task_trials_question = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_question',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  imagery_task_trials_word1 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  imagery_task_trials_word1.setVolume(8);
  imagery_task_trials_word2 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  imagery_task_trials_word2.setVolume(8);
  imagery_task_trials_instruction = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: (- 1),
      });
  imagery_task_trials_instruction.setVolume(8);
  imagery_task_trials_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  imagery_task_trials_order_instruction = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_order_instruction',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.13], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -5.0 
  });
  
  imagery_task_trials_img1 = new visual.ImageStim({
    win : psychoJS.window,
    name : 'imagery_task_trials_img1', units : undefined, 
    image : 'default.png', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, (- 0.25)], 
    draggable: false,
    size : [0.4, 0.25],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -6.0 
  });
  imagery_task_trials_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_proceed',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.45)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -7.0 
  });
  
  // Initialize components for Routine "imagery_task_trials_vividness"
  imagery_task_trials_vividnessClock = new util.Clock();
  imagery_task_trials_vividness_question = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_vividness_question',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  imagery_task_trials_vividness_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_vividness_text',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  imagery_task_trials_vividness_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "resting"
  restingClock = new util.Clock();
  imagery_task_trials_rest = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_trials_rest',
    text: '',
    font: 'Open Sans',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.1,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('white'),  opacity: undefined,
    depth: 0.0 
  });
  
  // Initialize components for Routine "imagery_task_prepare"
  imagery_task_prepareClock = new util.Clock();
  imagery_task_prepare_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_prepare_title',
    text: 'Part 2/4 : Test of Imagination',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  imagery_task_prepare_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_prepare_text',
    text: "Bravo!\n\nNow let's start the tests.\n\nTake your time to answer each question.",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  imagery_task_prepare_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  imagery_task_prepare_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_prepare_proceed',
    text: 'When you are ready, press the < Space > key to proceed.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -3.0 
  });
  
  // Initialize components for Routine "imagery_task"
  imagery_taskClock = new util.Clock();
  imagery_task_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_title',
    text: 'Part 2/4: Test of Visual Mental Imagery',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  imagery_task_idx = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_idx',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.3], draggable: false, height: 0.05,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  imagery_task_word1 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  imagery_task_word1.setVolume(8);
  imagery_task_word2 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  imagery_task_word2.setVolume(8);
  imagery_task_instruction = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: (- 1),
      });
  imagery_task_instruction.setVolume(8);
  imagery_task_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_text',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -5.0 
  });
  
  key_resp_vmi = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "imagery_task_vividness"
  imagery_task_vividnessClock = new util.Clock();
  imagery_task_vividness_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_vividness_title',
    text: 'Give a score of 1 to 4 to rate the vividness of your image.\n',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  vividness = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "imagery_task_feedback"
  imagery_task_feedbackClock = new util.Clock();
  imagery_task_feedback_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_feedback_title',
    text: 'Part 2/4: Test of Visual Mental Imagery',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  // Run 'Begin Experiment' code from imagery_task_feedback_text
  msg='doh';
  colortext = 'black';
  
  imagery_task_feedback_text_1 = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_feedback_text_1',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('white'),  opacity: 1,
    depth: -2.0 
  });
  
  imagery_task_feedback_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  imagery_task_feedback_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'imagery_task_feedback_proceed',
    text: 'When you are ready, press the < Space > key to proceed.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -4.0 
  });
  
  // Initialize components for Routine "perception_instruction"
  perception_instructionClock = new util.Clock();
  perception_instruction_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_instruction_title',
    text: 'Part 3/4 : Test of Perception\n\n',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  perception_instruction_task = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_instruction_task',
    text: "In this third part, we are going to show you pictures of objects, food, celebrities, or written words that we have asked you to imagine throughout the part 2 of this test.\n\nDuring this part, you will answer the questions by comparing the visual characteristics perceived based on the pictures that you will be able to SEE.\n\nThe perception test takes approximately 10 minutes, with 60 trials in total.\n\nAttention, for the map test, select the city indicated by a red dot (whether it appears first or second).\n\nAfter each question, rate the level of CONFIDENCE of your answer:\n1: I am not at all confident in my answer.\n2: I'm a little confident about my answer.\n3: I am confident in my answer.\n4: I am absolutely sure of my answer.",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  perception_instruction_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  perception_instruction_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_instruction_proceed',
    text: 'Press < Space > to proceed. ',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -3.0 
  });
  
  // Initialize components for Routine "perception_task"
  perception_taskClock = new util.Clock();
  perception_task_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_title',
    text: 'Part 3: Test of Perception',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  perception_task_word1 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  perception_task_word1.setVolume(8);
  perception_task_img1 = new visual.ImageStim({
    win : psychoJS.window,
    name : 'perception_task_img1', units : undefined, 
    image : 'default.png', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, 0], 
    draggable: false,
    size : [1.16, 0.64],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -2.0 
  });
  perception_task_word2 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  perception_task_word2.setVolume(8);
  perception_task_img2 = new visual.ImageStim({
    win : psychoJS.window,
    name : 'perception_task_img2', units : undefined, 
    image : 'default.png', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, 0], 
    draggable: false,
    size : [1.16, 0.64],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -4.0 
  });
  perception_task_instruction = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: (- 1),
      });
  perception_task_instruction.setVolume(8);
  perception_task_img3 = new visual.ImageStim({
    win : psychoJS.window,
    name : 'perception_task_img3', units : undefined, 
    image : 'default.png', mask : undefined,
    anchor : 'center',
    ori : 0, 
    pos : [0, 0], 
    draggable: false,
    size : [1.16, 0.64],
    color : new util.Color([1, 1, 1]), opacity : 1,
    flipHoriz : false, flipVert : false,
    texRes : 128, interpolate : true, depth : -6.0 
  });
  perception = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  perception_task_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_text',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.45)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -8.0 
  });
  
  // Initialize components for Routine "perception_task_confidence"
  perception_task_confidenceClock = new util.Clock();
  perception_task_confidence_idx = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_confidence_idx',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.3], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  perception_task_confidence_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_confidence_text',
    text: "Give a score from 1 to 4 to assess your CONFIDENCE.\n\n1: I am not at all confident in my answer.\n2: I'm a little confident about my answer.\n3: I am confident in my answer.\n4: I am absolutely sure of my answer.",
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -1.0 
  });
  
  confidence = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "perception_task_feedback"
  perception_task_feedbackClock = new util.Clock();
  perception_task_feedback_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_feedback_title',
    text: 'Part 3/4 : Test of Perception',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  // Run 'Begin Experiment' code from perception_task_feedback_text
  msg='doh';
  colortext = 'black';
  
  perception_task_feedback_text_1 = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_feedback_text_1',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('white'),  opacity: 1,
    depth: -2.0 
  });
  
  perception_task_feedback_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  perception_task_feedback_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'perception_task_feedback_proceed',
    text: 'Press < Space > to proceed. ',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -4.0 
  });
  
  // Initialize components for Routine "sem_instruction"
  sem_instructionClock = new util.Clock();
  semantics_instruction_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_instruction_title',
    text: 'Part 4/4: Test of Semantics',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1.0,
    depth: 0.0 
  });
  
  semantics_instruction_task = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_instruction_task',
    text: 'In this last part, we will evaluate your association between abstract words.\n\nDuring this part, you will answer the questions by comparing the abstract characteristics based on the semantic relationship.\nFor each trial, you will hear two abstract words after hearing the instruction “Semantics”. Then, you will hear the indication semantic word. \nWhich of them is closer to the indication word?\nWe will start with a practice trial. \n\nThis Semantics Task will take about 3 minutes with 12 trials.',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1.0,
    depth: -1.0 
  });
  
  semantics_instruction_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  semantics_instruction_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_instruction_proceed',
    text: 'Press <Space> to proceed.',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1.0,
    depth: -3.0 
  });
  
  // Initialize components for Routine "semantics_task_trials"
  semantics_task_trialsClock = new util.Clock();
  semantics_task_trials_question = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_task_trials_question',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  semantics_task_trials_word1 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  semantics_task_trials_word1.setVolume(8);
  semantics_task_trials_word2 = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  semantics_task_trials_word2.setVolume(8);
  semantics_task_trials_instruction = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: (- 1),
      });
  semantics_task_trials_instruction.setVolume(8);
  semantics_task_trials_response = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  semantics_task_trials_order_instruction = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_task_trials_order_instruction',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.13], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: -5.0 
  });
  
  semantics_task_trials_proceed = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_task_trials_proceed',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, (- 0.4)], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('green'),  opacity: 1,
    depth: -6.0 
  });
  
  // Initialize components for Routine "sem_trial"
  sem_trialClock = new util.Clock();
  semantics_task_title = new visual.TextStim({
    win: psychoJS.window,
    name: 'semantics_task_title',
    text: 'Part 4/4: Test of Semantics',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.4], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1.0,
    depth: 0.0 
  });
  
  semanticBlock_idx = new visual.TextStim({
    win: psychoJS.window,
    name: 'semanticBlock_idx',
    text: '',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0.3], draggable: false, height: 0.04,  wrapWidth: undefined, ori: 0.0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1.0,
    depth: -1.0 
  });
  
  fixation = new visual.ShapeStim ({
    win: psychoJS.window, name: 'fixation', 
    vertices: 'cross', size:[0.03, 0.03],
    ori: 0.0, 
    pos: [0, 0], 
    draggable: false, 
    anchor: 'center', 
    lineWidth: 1.0, 
    lineColor: new util.Color('white'), 
    fillColor: new util.Color('white'), 
    colorSpace: 'rgb', 
    opacity: undefined, 
    depth: -2, 
    interpolate: true, 
  });
  
  field_s = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  field_s.setVolume(1.0);
  word1_s = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  word1_s.setVolume(1.0);
  word2_s = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: 2,
      });
  word2_s.setVolume(1.0);
  instr_s = new sound.Sound({
      win: psychoJS.window,
      value: 'A',
      secs: (- 1),
      });
  instr_s.setVolume(1.0);
  key_resp_vmi_s = new core.Keyboard({psychoJS: psychoJS, clock: new util.Clock(), waitForStart: true});
  
  // Initialize components for Routine "end_session"
  end_sessionClock = new util.Clock();
  end_session_text = new visual.TextStim({
    win: psychoJS.window,
    name: 'end_session_text',
    text: 'This is the end of tests. Thank you for your participation!\n\n\nFeel free to comment on the site or contact us:\n\n- Share your experience of life and your result.\n- Interested in an fMRI neuroimaging test for research.\n- ...\n\n\nWait for the program to close or press the <Space> key to exit after 5 seconds.',
    font: 'Arial',
    units: undefined, 
    pos: [0, 0], draggable: false, height: 0.03,  wrapWidth: undefined, ori: 0,
    languageStyle: 'LTR',
    color: new util.Color('black'),  opacity: 1,
    depth: 0.0 
  });
  
  // Create some handy timers
  globalClock = new util.Clock();  // to track the time since experiment started
  routineTimer = new util.CountdownTimer();  // to track time remaining of each (non-slip) routine
  
  return Scheduler.Event.NEXT;
}


var t;
var frameN;
var continueRoutine;
var resource_manageMaxDurationReached;
var resource_manageMaxDuration;
var resource_manageComponents;
function resource_manageRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'resource_manage' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    resource_manageClock.reset();
    routineTimer.reset();
    resource_manageMaxDurationReached = false;
    // update component parameters for each repeat
    psychoJS.experiment.addData('resource_manage.started', globalClock.getTime());
    resource_manageMaxDuration = null
    // keep track of which components have finished
    resource_manageComponents = [];
    
    for (const thisComponent of resource_manageComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function resource_manageRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'resource_manage' ---
    // get current time
    t = resource_manageClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of resource_manageComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function resource_manageRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'resource_manage' ---
    for (const thisComponent of resource_manageComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('resource_manage.stopped', globalClock.getTime());
    // the Routine "resource_manage" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var volume_testMaxDurationReached;
var _volume_response_allKeys;
var volume_testMaxDuration;
var volume_testComponents;
function volume_testRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'volume_test' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    volume_testClock.reset();
    routineTimer.reset();
    volume_testMaxDurationReached = false;
    // update component parameters for each repeat
    volume_response.keys = undefined;
    volume_response.rt = undefined;
    _volume_response_allKeys = [];
    volume_text.setAlignHoriz('left')
    sound_1.setVolume(8);
    psychoJS.experiment.addData('volume_test.started', globalClock.getTime());
    volume_testMaxDuration = null
    // keep track of which components have finished
    volume_testComponents = [];
    volume_testComponents.push(volume_title);
    volume_testComponents.push(volume_response);
    volume_testComponents.push(volume_text);
    volume_testComponents.push(volume_continue);
    volume_testComponents.push(sound_1);
    
    for (const thisComponent of volume_testComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function volume_testRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'volume_test' ---
    // get current time
    t = volume_testClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *volume_title* updates
    if (t >= 0.0 && volume_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      volume_title.tStart = t;  // (not accounting for frame time here)
      volume_title.frameNStart = frameN;  // exact frame index
      
      volume_title.setAutoDraw(true);
    }
    
    
    // *volume_response* updates
    if (t >= 0.0 && volume_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      volume_response.tStart = t;  // (not accounting for frame time here)
      volume_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { volume_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { volume_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { volume_response.clearEvents(); });
    }
    
    if (volume_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = volume_response.getKeys({keyList: ['space'], waitRelease: false});
      _volume_response_allKeys = _volume_response_allKeys.concat(theseKeys);
      if (_volume_response_allKeys.length > 0) {
        volume_response.keys = _volume_response_allKeys[_volume_response_allKeys.length - 1].name;  // just the last key pressed
        volume_response.rt = _volume_response_allKeys[_volume_response_allKeys.length - 1].rt;
        volume_response.duration = _volume_response_allKeys[_volume_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *volume_text* updates
    if (t >= 0.0 && volume_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      volume_text.tStart = t;  // (not accounting for frame time here)
      volume_text.frameNStart = frameN;  // exact frame index
      
      volume_text.setAutoDraw(true);
    }
    
    
    // *volume_continue* updates
    if (t >= 0.0 && volume_continue.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      volume_continue.tStart = t;  // (not accounting for frame time here)
      volume_continue.frameNStart = frameN;  // exact frame index
      
      volume_continue.setAutoDraw(true);
    }
    
    // start/stop sound_1
    if (t >= 0.0 && sound_1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      sound_1.tStart = t;  // (not accounting for frame time here)
      sound_1.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ sound_1.play(); });  // screen flip
      sound_1.status = PsychoJS.Status.STARTED;
    }
    if (t >= (sound_1.getDuration() + sound_1.tStart)     && sound_1.status === PsychoJS.Status.STARTED) {
      sound_1.stop();  // stop the sound (if longer than duration)
      sound_1.status = PsychoJS.Status.FINISHED;
    }
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of volume_testComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function volume_testRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'volume_test' ---
    for (const thisComponent of volume_testComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('volume_test.stopped', globalClock.getTime());
    volume_response.stop();
    sound_1.stop();  // ensure sound has stopped at end of Routine
    // the Routine "volume_test" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var key_testMaxDurationReached;
var _key_test_show_allKeys;
var key_testMaxDuration;
var key_testComponents;
function key_testRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'key_test' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    key_testClock.reset();
    routineTimer.reset();
    key_testMaxDurationReached = false;
    // update component parameters for each repeat
    key_test_show.keys = undefined;
    key_test_show.rt = undefined;
    _key_test_show_allKeys = [];
    // Run 'Begin Routine' code from key_test_present
    key_test_text.setAlignHoriz('left')
    key_test_display.text = '';
    psychoJS.experiment.addData('key_test.started', globalClock.getTime());
    key_testMaxDuration = null
    // keep track of which components have finished
    key_testComponents = [];
    key_testComponents.push(key_test_title);
    key_testComponents.push(key_test_text);
    key_testComponents.push(key_test_display);
    key_testComponents.push(key_test_show);
    key_testComponents.push(key_test_continue);
    key_testComponents.push(key_test_image);
    
    for (const thisComponent of key_testComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


var keysGet;
function key_testRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'key_test' ---
    // get current time
    t = key_testClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *key_test_title* updates
    if (t >= 0.0 && key_test_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_title.tStart = t;  // (not accounting for frame time here)
      key_test_title.frameNStart = frameN;  // exact frame index
      
      key_test_title.setAutoDraw(true);
    }
    
    
    // *key_test_text* updates
    if (t >= 0.0 && key_test_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_text.tStart = t;  // (not accounting for frame time here)
      key_test_text.frameNStart = frameN;  // exact frame index
      
      key_test_text.setAutoDraw(true);
    }
    
    
    if (key_test_display.status === PsychoJS.Status.STARTED){ // only update if being drawn
      key_test_display.setText('', false);
    }
    
    // *key_test_display* updates
    if (t >= 0.0 && key_test_display.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_display.tStart = t;  // (not accounting for frame time here)
      key_test_display.frameNStart = frameN;  // exact frame index
      
      key_test_display.setAutoDraw(true);
    }
    
    
    // *key_test_show* updates
    if (t >= 0.0 && key_test_show.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_show.tStart = t;  // (not accounting for frame time here)
      key_test_show.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { key_test_show.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { key_test_show.start(); }); // start on screen flip
    }
    
    if (key_test_show.status === PsychoJS.Status.STARTED) {
      let theseKeys = key_test_show.getKeys({keyList: ['1', '2', '3', '4', '5', 'left', 'right', 'space', 't', 'b', 'y', 'g', 'r', 'comma', 'return'], waitRelease: false});
      _key_test_show_allKeys = _key_test_show_allKeys.concat(theseKeys);
      if (_key_test_show_allKeys.length > 0) {
        key_test_show.keys = _key_test_show_allKeys[_key_test_show_allKeys.length - 1].name;  // just the last key pressed
        key_test_show.rt = _key_test_show_allKeys[_key_test_show_allKeys.length - 1].rt;
        key_test_show.duration = _key_test_show_allKeys[_key_test_show_allKeys.length - 1].duration;
      }
    }
    
    // Run 'Each Frame' code from key_test_present
    let theseKeys = psychoJS.eventManager.getKeys();
    
    if (theseKeys.length > 0) {
      keysGet = theseKeys[theseKeys.length-1]; 
      }
    
    
    if (keysGet == 'return'){
        continueRoutine = false;
    } else {
        key_test_display.text = keysGet;
    }
    
    // *key_test_continue* updates
    if (t >= 0.0 && key_test_continue.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_continue.tStart = t;  // (not accounting for frame time here)
      key_test_continue.frameNStart = frameN;  // exact frame index
      
      key_test_continue.setAutoDraw(true);
    }
    
    
    // *key_test_image* updates
    if (t >= 0.0 && key_test_image.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_test_image.tStart = t;  // (not accounting for frame time here)
      key_test_image.frameNStart = frameN;  // exact frame index
      
      key_test_image.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of key_testComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function key_testRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'key_test' ---
    for (const thisComponent of key_testComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('key_test.stopped', globalClock.getTime());
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(key_test_show.corr, level);
    }
    psychoJS.experiment.addData('key_test_show.keys', key_test_show.keys);
    if (typeof key_test_show.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('key_test_show.rt', key_test_show.rt);
        psychoJS.experiment.addData('key_test_show.duration', key_test_show.duration);
        }
    
    key_test_show.stop();
    // the Routine "key_test" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var begin_instructionMaxDurationReached;
var _begin_response_allKeys;
var begin_instructionMaxDuration;
var begin_instructionComponents;
function begin_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'begin_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    begin_instructionClock.reset();
    routineTimer.reset();
    begin_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    begin_response.keys = undefined;
    begin_response.rt = undefined;
    _begin_response_allKeys = [];
    psychoJS.experiment.addData('begin_instruction.started', globalClock.getTime());
    begin_instructionMaxDuration = null
    // keep track of which components have finished
    begin_instructionComponents = [];
    begin_instructionComponents.push(begin_text);
    begin_instructionComponents.push(begin_response);
    begin_instructionComponents.push(begin_continue);
    
    for (const thisComponent of begin_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function begin_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'begin_instruction' ---
    // get current time
    t = begin_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *begin_text* updates
    if (t >= 0.0 && begin_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      begin_text.tStart = t;  // (not accounting for frame time here)
      begin_text.frameNStart = frameN;  // exact frame index
      
      begin_text.setAutoDraw(true);
    }
    
    
    // *begin_response* updates
    if (t >= 0.0 && begin_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      begin_response.tStart = t;  // (not accounting for frame time here)
      begin_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { begin_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { begin_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { begin_response.clearEvents(); });
    }
    
    if (begin_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = begin_response.getKeys({keyList: ['space'], waitRelease: false});
      _begin_response_allKeys = _begin_response_allKeys.concat(theseKeys);
      if (_begin_response_allKeys.length > 0) {
        begin_response.keys = _begin_response_allKeys[_begin_response_allKeys.length - 1].name;  // just the last key pressed
        begin_response.rt = _begin_response_allKeys[_begin_response_allKeys.length - 1].rt;
        begin_response.duration = _begin_response_allKeys[_begin_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *begin_continue* updates
    if (t >= 0.0 && begin_continue.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      begin_continue.tStart = t;  // (not accounting for frame time here)
      begin_continue.frameNStart = frameN;  // exact frame index
      
      begin_continue.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of begin_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function begin_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'begin_instruction' ---
    for (const thisComponent of begin_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('begin_instruction.stopped', globalClock.getTime());
    begin_response.stop();
    // the Routine "begin_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var vivid_instructionMaxDurationReached;
var _vivid_response_allKeys;
var vivid_instructionMaxDuration;
var vivid_instructionComponents;
function vivid_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'vivid_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    vivid_instructionClock.reset();
    routineTimer.reset();
    vivid_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    vivid_response.keys = undefined;
    vivid_response.rt = undefined;
    _vivid_response_allKeys = [];
    vivid_rating.setAlignHoriz('left')
    vivid_title.setAlignHoriz('left')
    psychoJS.experiment.addData('vivid_instruction.started', globalClock.getTime());
    vivid_instructionMaxDuration = null
    // keep track of which components have finished
    vivid_instructionComponents = [];
    vivid_instructionComponents.push(vivid_title);
    vivid_instructionComponents.push(vivid_response);
    vivid_instructionComponents.push(vivid_mona_lisa);
    vivid_instructionComponents.push(vivid_rating);
    vivid_instructionComponents.push(vivid_image);
    vivid_instructionComponents.push(vivid_text);
    
    for (const thisComponent of vivid_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function vivid_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'vivid_instruction' ---
    // get current time
    t = vivid_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *vivid_title* updates
    if (t >= 0.0 && vivid_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_title.tStart = t;  // (not accounting for frame time here)
      vivid_title.frameNStart = frameN;  // exact frame index
      
      vivid_title.setAutoDraw(true);
    }
    
    
    // *vivid_response* updates
    if (t >= 0.0 && vivid_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_response.tStart = t;  // (not accounting for frame time here)
      vivid_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { vivid_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { vivid_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { vivid_response.clearEvents(); });
    }
    
    if (vivid_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = vivid_response.getKeys({keyList: ['1', '2', '3', '4', '5'], waitRelease: false});
      _vivid_response_allKeys = _vivid_response_allKeys.concat(theseKeys);
      if (_vivid_response_allKeys.length > 0) {
        vivid_response.keys = _vivid_response_allKeys[_vivid_response_allKeys.length - 1].name;  // just the last key pressed
        vivid_response.rt = _vivid_response_allKeys[_vivid_response_allKeys.length - 1].rt;
        vivid_response.duration = _vivid_response_allKeys[_vivid_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *vivid_mona_lisa* updates
    if (t >= 0.5 && vivid_mona_lisa.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_mona_lisa.tStart = t;  // (not accounting for frame time here)
      vivid_mona_lisa.frameNStart = frameN;  // exact frame index
      
      vivid_mona_lisa.setAutoDraw(true);
    }
    
    
    // *vivid_rating* updates
    if (t >= 3 && vivid_rating.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_rating.tStart = t;  // (not accounting for frame time here)
      vivid_rating.frameNStart = frameN;  // exact frame index
      
      vivid_rating.setAutoDraw(true);
    }
    
    
    // *vivid_image* updates
    if (t >= 3 && vivid_image.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_image.tStart = t;  // (not accounting for frame time here)
      vivid_image.frameNStart = frameN;  // exact frame index
      
      vivid_image.setAutoDraw(true);
    }
    
    
    // *vivid_text* updates
    if (t >= 4 && vivid_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vivid_text.tStart = t;  // (not accounting for frame time here)
      vivid_text.frameNStart = frameN;  // exact frame index
      
      vivid_text.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of vivid_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function vivid_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'vivid_instruction' ---
    for (const thisComponent of vivid_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('vivid_instruction.stopped', globalClock.getTime());
    vivid_response.stop();
    // the Routine "vivid_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var vviq_instructionMaxDurationReached;
var _vviq_instruction_response_allKeys;
var vviq_instructionMaxDuration;
var vviq_instructionComponents;
function vviq_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'vviq_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    vviq_instructionClock.reset();
    routineTimer.reset();
    vviq_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    vviq_instruction_response.keys = undefined;
    vviq_instruction_response.rt = undefined;
    _vviq_instruction_response_allKeys = [];
    psychoJS.experiment.addData('vviq_instruction.started', globalClock.getTime());
    vviq_instructionMaxDuration = null
    // keep track of which components have finished
    vviq_instructionComponents = [];
    vviq_instructionComponents.push(vviq_instruction_title);
    vviq_instructionComponents.push(vviq_instruction_text);
    vviq_instructionComponents.push(vviq_instruction_proceed);
    vviq_instructionComponents.push(vviq_instruction_response);
    vviq_instructionComponents.push(vviq_instruction_good);
    
    for (const thisComponent of vviq_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function vviq_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'vviq_instruction' ---
    // get current time
    t = vviq_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *vviq_instruction_title* updates
    if (t >= 0.0 && vviq_instruction_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_instruction_title.tStart = t;  // (not accounting for frame time here)
      vviq_instruction_title.frameNStart = frameN;  // exact frame index
      
      vviq_instruction_title.setAutoDraw(true);
    }
    
    
    // *vviq_instruction_text* updates
    if (t >= 0.0 && vviq_instruction_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_instruction_text.tStart = t;  // (not accounting for frame time here)
      vviq_instruction_text.frameNStart = frameN;  // exact frame index
      
      vviq_instruction_text.setAutoDraw(true);
    }
    
    
    // *vviq_instruction_proceed* updates
    if (t >= 0.0 && vviq_instruction_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_instruction_proceed.tStart = t;  // (not accounting for frame time here)
      vviq_instruction_proceed.frameNStart = frameN;  // exact frame index
      
      vviq_instruction_proceed.setAutoDraw(true);
    }
    
    
    // *vviq_instruction_response* updates
    if (t >= 0.0 && vviq_instruction_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_instruction_response.tStart = t;  // (not accounting for frame time here)
      vviq_instruction_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { vviq_instruction_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { vviq_instruction_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { vviq_instruction_response.clearEvents(); });
    }
    
    if (vviq_instruction_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = vviq_instruction_response.getKeys({keyList: ['space'], waitRelease: false});
      _vviq_instruction_response_allKeys = _vviq_instruction_response_allKeys.concat(theseKeys);
      if (_vviq_instruction_response_allKeys.length > 0) {
        vviq_instruction_response.keys = _vviq_instruction_response_allKeys[_vviq_instruction_response_allKeys.length - 1].name;  // just the last key pressed
        vviq_instruction_response.rt = _vviq_instruction_response_allKeys[_vviq_instruction_response_allKeys.length - 1].rt;
        vviq_instruction_response.duration = _vviq_instruction_response_allKeys[_vviq_instruction_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *vviq_instruction_good* updates
    if (t >= 0.0 && vviq_instruction_good.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_instruction_good.tStart = t;  // (not accounting for frame time here)
      vviq_instruction_good.frameNStart = frameN;  // exact frame index
      
      vviq_instruction_good.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of vviq_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function vviq_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'vviq_instruction' ---
    for (const thisComponent of vviq_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('vviq_instruction.stopped', globalClock.getTime());
    vviq_instruction_response.stop();
    // the Routine "vviq_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var vviq_trials;
function vviq_trialsLoopBegin(vviq_trialsLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    vviq_trials = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.SEQUENTIAL,
      extraInfo: expInfo, originPath: undefined,
      trialList: 'vviq_task_online.xlsx',
      seed: undefined, name: 'vviq_trials'
    });
    psychoJS.experiment.addLoop(vviq_trials); // add the loop to the experiment
    currentLoop = vviq_trials;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisVviq_trial of vviq_trials) {
      snapshot = vviq_trials.getSnapshot();
      vviq_trialsLoopScheduler.add(importConditions(snapshot));
      vviq_trialsLoopScheduler.add(vviq_taskRoutineBegin(snapshot));
      vviq_trialsLoopScheduler.add(vviq_taskRoutineEachFrame());
      vviq_trialsLoopScheduler.add(vviq_taskRoutineEnd(snapshot));
      vviq_trialsLoopScheduler.add(vviq_trialsLoopEndIteration(vviq_trialsLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function vviq_trialsLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(vviq_trials);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function vviq_trialsLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var imagery_domain;
function imagery_domainLoopBegin(imagery_domainLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    imagery_domain = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.RANDOM,
      extraInfo: expInfo, originPath: undefined,
      trialList: 'Imagery_domain_selection.xlsx',
      seed: undefined, name: 'imagery_domain'
    });
    psychoJS.experiment.addLoop(imagery_domain); // add the loop to the experiment
    currentLoop = imagery_domain;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisImagery_domain of imagery_domain) {
      snapshot = imagery_domain.getSnapshot();
      imagery_domainLoopScheduler.add(importConditions(snapshot));
      const imagery_trialsLoopScheduler = new Scheduler(psychoJS);
      imagery_domainLoopScheduler.add(imagery_trialsLoopBegin(imagery_trialsLoopScheduler, snapshot));
      imagery_domainLoopScheduler.add(imagery_trialsLoopScheduler);
      imagery_domainLoopScheduler.add(imagery_trialsLoopEnd);
      imagery_domainLoopScheduler.add(imagery_task_prepareRoutineBegin(snapshot));
      imagery_domainLoopScheduler.add(imagery_task_prepareRoutineEachFrame());
      imagery_domainLoopScheduler.add(imagery_task_prepareRoutineEnd(snapshot));
      const imagery_task_itemsLoopScheduler = new Scheduler(psychoJS);
      imagery_domainLoopScheduler.add(imagery_task_itemsLoopBegin(imagery_task_itemsLoopScheduler, snapshot));
      imagery_domainLoopScheduler.add(imagery_task_itemsLoopScheduler);
      imagery_domainLoopScheduler.add(imagery_task_itemsLoopEnd);
      imagery_domainLoopScheduler.add(imagery_task_feedbackRoutineBegin(snapshot));
      imagery_domainLoopScheduler.add(imagery_task_feedbackRoutineEachFrame());
      imagery_domainLoopScheduler.add(imagery_task_feedbackRoutineEnd(snapshot));
      imagery_domainLoopScheduler.add(imagery_domainLoopEndIteration(imagery_domainLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


var imagery_trials;
function imagery_trialsLoopBegin(imagery_trialsLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    imagery_trials = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.SEQUENTIAL,
      extraInfo: expInfo, originPath: undefined,
      trialList: TrialHandler.importConditions(psychoJS.serverManager, 'ImageryPractice_en.xlsx', practice_idx),
      seed: undefined, name: 'imagery_trials'
    });
    psychoJS.experiment.addLoop(imagery_trials); // add the loop to the experiment
    currentLoop = imagery_trials;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisImagery_trial of imagery_trials) {
      snapshot = imagery_trials.getSnapshot();
      imagery_trialsLoopScheduler.add(importConditions(snapshot));
      imagery_trialsLoopScheduler.add(imagery_task_trialsRoutineBegin(snapshot));
      imagery_trialsLoopScheduler.add(imagery_task_trialsRoutineEachFrame());
      imagery_trialsLoopScheduler.add(imagery_task_trialsRoutineEnd(snapshot));
      imagery_trialsLoopScheduler.add(imagery_task_trials_vividnessRoutineBegin(snapshot));
      imagery_trialsLoopScheduler.add(imagery_task_trials_vividnessRoutineEachFrame());
      imagery_trialsLoopScheduler.add(imagery_task_trials_vividnessRoutineEnd(snapshot));
      imagery_trialsLoopScheduler.add(restingRoutineBegin(snapshot));
      imagery_trialsLoopScheduler.add(restingRoutineEachFrame());
      imagery_trialsLoopScheduler.add(restingRoutineEnd(snapshot));
      imagery_trialsLoopScheduler.add(imagery_trialsLoopEndIteration(imagery_trialsLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function imagery_trialsLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(imagery_trials);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function imagery_trialsLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var imagery_task_items;
function imagery_task_itemsLoopBegin(imagery_task_itemsLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    imagery_task_items = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.RANDOM,
      extraInfo: expInfo, originPath: undefined,
      trialList: TrialHandler.importConditions(psychoJS.serverManager, 'ImageryItems_en.xlsx', trialRow),
      seed: undefined, name: 'imagery_task_items'
    });
    psychoJS.experiment.addLoop(imagery_task_items); // add the loop to the experiment
    currentLoop = imagery_task_items;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisImagery_task_item of imagery_task_items) {
      snapshot = imagery_task_items.getSnapshot();
      imagery_task_itemsLoopScheduler.add(importConditions(snapshot));
      imagery_task_itemsLoopScheduler.add(imagery_taskRoutineBegin(snapshot));
      imagery_task_itemsLoopScheduler.add(imagery_taskRoutineEachFrame());
      imagery_task_itemsLoopScheduler.add(imagery_taskRoutineEnd(snapshot));
      imagery_task_itemsLoopScheduler.add(imagery_task_vividnessRoutineBegin(snapshot));
      imagery_task_itemsLoopScheduler.add(imagery_task_vividnessRoutineEachFrame());
      imagery_task_itemsLoopScheduler.add(imagery_task_vividnessRoutineEnd(snapshot));
      imagery_task_itemsLoopScheduler.add(restingRoutineBegin(snapshot));
      imagery_task_itemsLoopScheduler.add(restingRoutineEachFrame());
      imagery_task_itemsLoopScheduler.add(restingRoutineEnd(snapshot));
      imagery_task_itemsLoopScheduler.add(imagery_task_itemsLoopEndIteration(imagery_task_itemsLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function imagery_task_itemsLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(imagery_task_items);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function imagery_task_itemsLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


async function imagery_domainLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(imagery_domain);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function imagery_domainLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var perception_task_trial;
function perception_task_trialLoopBegin(perception_task_trialLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    perception_task_trial = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.RANDOM,
      extraInfo: expInfo, originPath: undefined,
      trialList: 'PerceptionItems_en.xlsx',
      seed: undefined, name: 'perception_task_trial'
    });
    psychoJS.experiment.addLoop(perception_task_trial); // add the loop to the experiment
    currentLoop = perception_task_trial;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisPerception_task_trial of perception_task_trial) {
      snapshot = perception_task_trial.getSnapshot();
      perception_task_trialLoopScheduler.add(importConditions(snapshot));
      perception_task_trialLoopScheduler.add(perception_taskRoutineBegin(snapshot));
      perception_task_trialLoopScheduler.add(perception_taskRoutineEachFrame());
      perception_task_trialLoopScheduler.add(perception_taskRoutineEnd(snapshot));
      perception_task_trialLoopScheduler.add(perception_task_confidenceRoutineBegin(snapshot));
      perception_task_trialLoopScheduler.add(perception_task_confidenceRoutineEachFrame());
      perception_task_trialLoopScheduler.add(perception_task_confidenceRoutineEnd(snapshot));
      perception_task_trialLoopScheduler.add(restingRoutineBegin(snapshot));
      perception_task_trialLoopScheduler.add(restingRoutineEachFrame());
      perception_task_trialLoopScheduler.add(restingRoutineEnd(snapshot));
      perception_task_trialLoopScheduler.add(perception_task_trialLoopEndIteration(perception_task_trialLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function perception_task_trialLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(perception_task_trial);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function perception_task_trialLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var trials;
function trialsLoopBegin(trialsLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    trials = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.SEQUENTIAL,
      extraInfo: expInfo, originPath: undefined,
      trialList: 'SemanticsPractice_en.xlsx',
      seed: undefined, name: 'trials'
    });
    psychoJS.experiment.addLoop(trials); // add the loop to the experiment
    currentLoop = trials;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisTrial of trials) {
      snapshot = trials.getSnapshot();
      trialsLoopScheduler.add(importConditions(snapshot));
      trialsLoopScheduler.add(semantics_task_trialsRoutineBegin(snapshot));
      trialsLoopScheduler.add(semantics_task_trialsRoutineEachFrame());
      trialsLoopScheduler.add(semantics_task_trialsRoutineEnd(snapshot));
      trialsLoopScheduler.add(trialsLoopEndIteration(trialsLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function trialsLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(trials);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function trialsLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var semanticBlock;
function semanticBlockLoopBegin(semanticBlockLoopScheduler, snapshot) {
  return async function() {
    TrialHandler.fromSnapshot(snapshot); // update internal variables (.thisN etc) of the loop
    
    // set up handler to look after randomisation of conditions etc
    semanticBlock = new TrialHandler({
      psychoJS: psychoJS,
      nReps: 1, method: TrialHandler.Method.SEQUENTIAL,
      extraInfo: expInfo, originPath: undefined,
      trialList: 'SemanticItems.xlsx',
      seed: undefined, name: 'semanticBlock'
    });
    psychoJS.experiment.addLoop(semanticBlock); // add the loop to the experiment
    currentLoop = semanticBlock;  // we're now the current loop
    
    // Schedule all the trials in the trialList:
    for (const thisSemanticBlock of semanticBlock) {
      snapshot = semanticBlock.getSnapshot();
      semanticBlockLoopScheduler.add(importConditions(snapshot));
      semanticBlockLoopScheduler.add(sem_trialRoutineBegin(snapshot));
      semanticBlockLoopScheduler.add(sem_trialRoutineEachFrame());
      semanticBlockLoopScheduler.add(sem_trialRoutineEnd(snapshot));
      semanticBlockLoopScheduler.add(semanticBlockLoopEndIteration(semanticBlockLoopScheduler, snapshot));
    }
    
    return Scheduler.Event.NEXT;
  }
}


async function semanticBlockLoopEnd() {
  // terminate loop
  psychoJS.experiment.removeLoop(semanticBlock);
  // update the current loop from the ExperimentHandler
  if (psychoJS.experiment._unfinishedLoops.length>0)
    currentLoop = psychoJS.experiment._unfinishedLoops.at(-1);
  else
    currentLoop = psychoJS.experiment;  // so we use addData from the experiment
  return Scheduler.Event.NEXT;
}


function semanticBlockLoopEndIteration(scheduler, snapshot) {
  // ------Prepare for next entry------
  return async function () {
    if (typeof snapshot !== 'undefined') {
      // ------Check if user ended loop early------
      if (snapshot.finished) {
        // Check for and save orphaned data
        if (psychoJS.experiment.isEntryEmpty()) {
          psychoJS.experiment.nextEntry(snapshot);
        }
        scheduler.stop();
      } else {
        psychoJS.experiment.nextEntry(snapshot);
      }
    return Scheduler.Event.NEXT;
    }
  };
}


var vviq_taskMaxDurationReached;
var _key_resp_vviq_allKeys;
var vviq_taskMaxDuration;
var vviq_taskComponents;
function vviq_taskRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'vviq_task' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    vviq_taskClock.reset();
    routineTimer.reset();
    vviq_taskMaxDurationReached = false;
    // update component parameters for each repeat
    key_resp_vviq.keys = undefined;
    key_resp_vviq.rt = undefined;
    _key_resp_vviq_allKeys = [];
    vviq_task_idx.setText(vviq_idx);
    vviq_task_item.setText(item);
    vviq_task_senario.setText(senario);
    psychoJS.experiment.addData('vviq_task.started', globalClock.getTime());
    vviq_taskMaxDuration = null
    // keep track of which components have finished
    vviq_taskComponents = [];
    vviq_taskComponents.push(vviq_task_title);
    vviq_taskComponents.push(key_resp_vviq);
    vviq_taskComponents.push(vviq_task_score);
    vviq_taskComponents.push(vviq_task_idx);
    vviq_taskComponents.push(vviq_task_item);
    vviq_taskComponents.push(vviq_task_senario);
    
    for (const thisComponent of vviq_taskComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function vviq_taskRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'vviq_task' ---
    // get current time
    t = vviq_taskClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *vviq_task_title* updates
    if (t >= 0.0 && vviq_task_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_task_title.tStart = t;  // (not accounting for frame time here)
      vviq_task_title.frameNStart = frameN;  // exact frame index
      
      vviq_task_title.setAutoDraw(true);
    }
    
    
    // *key_resp_vviq* updates
    if (t >= 0 && key_resp_vviq.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_resp_vviq.tStart = t;  // (not accounting for frame time here)
      key_resp_vviq.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { key_resp_vviq.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vviq.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vviq.clearEvents(); });
    }
    
    if (key_resp_vviq.status === PsychoJS.Status.STARTED) {
      let theseKeys = key_resp_vviq.getKeys({keyList: ['1', '2', '3', '4', '5'], waitRelease: false});
      _key_resp_vviq_allKeys = _key_resp_vviq_allKeys.concat(theseKeys);
      if (_key_resp_vviq_allKeys.length > 0) {
        key_resp_vviq.keys = _key_resp_vviq_allKeys[_key_resp_vviq_allKeys.length - 1].name;  // just the last key pressed
        key_resp_vviq.rt = _key_resp_vviq_allKeys[_key_resp_vviq_allKeys.length - 1].rt;
        key_resp_vviq.duration = _key_resp_vviq_allKeys[_key_resp_vviq_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *vviq_task_score* updates
    if (t >= 0.0 && vviq_task_score.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_task_score.tStart = t;  // (not accounting for frame time here)
      vviq_task_score.frameNStart = frameN;  // exact frame index
      
      vviq_task_score.setAutoDraw(true);
    }
    
    
    // *vviq_task_idx* updates
    if (t >= 0.0 && vviq_task_idx.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_task_idx.tStart = t;  // (not accounting for frame time here)
      vviq_task_idx.frameNStart = frameN;  // exact frame index
      
      vviq_task_idx.setAutoDraw(true);
    }
    
    
    // *vviq_task_item* updates
    if (t >= 0.0 && vviq_task_item.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_task_item.tStart = t;  // (not accounting for frame time here)
      vviq_task_item.frameNStart = frameN;  // exact frame index
      
      vviq_task_item.setAutoDraw(true);
    }
    
    
    // *vviq_task_senario* updates
    if (t >= 0.0 && vviq_task_senario.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_task_senario.tStart = t;  // (not accounting for frame time here)
      vviq_task_senario.frameNStart = frameN;  // exact frame index
      
      vviq_task_senario.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of vviq_taskComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function vviq_taskRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'vviq_task' ---
    for (const thisComponent of vviq_taskComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('vviq_task.stopped', globalClock.getTime());
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(key_resp_vviq.corr, level);
    }
    psychoJS.experiment.addData('key_resp_vviq.keys', key_resp_vviq.keys);
    if (typeof key_resp_vviq.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('key_resp_vviq.rt', key_resp_vviq.rt);
        psychoJS.experiment.addData('key_resp_vviq.duration', key_resp_vviq.duration);
        routineTimer.reset();
        }
    
    key_resp_vviq.stop();
    // the Routine "vviq_task" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var vviq_feedbackMaxDurationReached;
var responses;
var subBlock;
var _vviq_feedback_response_allKeys;
var vviq_feedbackMaxDuration;
var vviq_feedbackComponents;
function vviq_feedbackRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'vviq_feedback' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    vviq_feedbackClock.reset();
    routineTimer.reset();
    vviq_feedbackMaxDurationReached = false;
    // update component parameters for each repeat
    // Run 'Begin Routine' code from vviq_feedback_text
    responses = psychoJS.experiment._trialsData;
    subBlock = responses.slice(previousLength);
    console.log("subBlock est : ");
    console.log(subBlock);
    
    let myvviqscore = 0;
    subBlock.forEach(entry => {
        let value = entry["key_resp_vviq.keys"];
        // Check if the value is a number or can be converted to a number
        if (!isNaN(value)) {
            myvviqscore += parseInt(value);
        } else {
            console.log('Invalid value:', value);
        }
    });
    
    if (myvviqscore > 70) {
        msg = "For this vividness test, you got a total score of "+ myvviqscore.toFixed(0) + " out of 80. I invite you to comment on our website and write us an email at the end of the session.";
        colortext = 'green';
    } else if (myvviqscore >=30) {
        msg = "For this vividness test, you got a total score of "+ myvviqscore.toFixed(0) + " out of 80. I invite you to comment on our website and write us an email at the end of the session.";
        colortext = 'green';
    } else {
        msg = "For this vividness test, you got a total score of "+ myvviqscore.toFixed(0) + " out of 80. I invite you to comment on our website and write us an email at the end of the session.";
        colortext = 'red';
    }
    
    vviq_feedback_response.keys = undefined;
    vviq_feedback_response.rt = undefined;
    _vviq_feedback_response_allKeys = [];
    psychoJS.experiment.addData('vviq_feedback.started', globalClock.getTime());
    vviq_feedbackMaxDuration = null
    // keep track of which components have finished
    vviq_feedbackComponents = [];
    vviq_feedbackComponents.push(vviq_feedback_title);
    vviq_feedbackComponents.push(vviq_feedback_proceed);
    vviq_feedbackComponents.push(vviq_feedback_response);
    vviq_feedbackComponents.push(vviq_feedback_result);
    
    for (const thisComponent of vviq_feedbackComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function vviq_feedbackRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'vviq_feedback' ---
    // get current time
    t = vviq_feedbackClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *vviq_feedback_title* updates
    if (t >= 0.0 && vviq_feedback_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_feedback_title.tStart = t;  // (not accounting for frame time here)
      vviq_feedback_title.frameNStart = frameN;  // exact frame index
      
      vviq_feedback_title.setAutoDraw(true);
    }
    
    
    // *vviq_feedback_proceed* updates
    if (t >= 0 && vviq_feedback_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_feedback_proceed.tStart = t;  // (not accounting for frame time here)
      vviq_feedback_proceed.frameNStart = frameN;  // exact frame index
      
      vviq_feedback_proceed.setAutoDraw(true);
    }
    
    
    // *vviq_feedback_response* updates
    if (t >= 0 && vviq_feedback_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_feedback_response.tStart = t;  // (not accounting for frame time here)
      vviq_feedback_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { vviq_feedback_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { vviq_feedback_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { vviq_feedback_response.clearEvents(); });
    }
    
    if (vviq_feedback_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = vviq_feedback_response.getKeys({keyList: ['space'], waitRelease: false});
      _vviq_feedback_response_allKeys = _vviq_feedback_response_allKeys.concat(theseKeys);
      if (_vviq_feedback_response_allKeys.length > 0) {
        vviq_feedback_response.keys = _vviq_feedback_response_allKeys[_vviq_feedback_response_allKeys.length - 1].name;  // just the last key pressed
        vviq_feedback_response.rt = _vviq_feedback_response_allKeys[_vviq_feedback_response_allKeys.length - 1].rt;
        vviq_feedback_response.duration = _vviq_feedback_response_allKeys[_vviq_feedback_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    if (vviq_feedback_result.status === PsychoJS.Status.STARTED){ // only update if being drawn
      vviq_feedback_result.setColor(new util.Color(colortext), false);
      vviq_feedback_result.setText(msg, false);
    }
    
    // *vviq_feedback_result* updates
    if (t >= 0.0 && vviq_feedback_result.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vviq_feedback_result.tStart = t;  // (not accounting for frame time here)
      vviq_feedback_result.frameNStart = frameN;  // exact frame index
      
      vviq_feedback_result.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of vviq_feedbackComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function vviq_feedbackRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'vviq_feedback' ---
    for (const thisComponent of vviq_feedbackComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('vviq_feedback.stopped', globalClock.getTime());
    vviq_feedback_response.stop();
    // the Routine "vviq_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_instructionMaxDurationReached;
var _imagery_instruction_proceed_2_allKeys;
var imagery_instructionMaxDuration;
var imagery_instructionComponents;
function imagery_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_instructionClock.reset();
    routineTimer.reset();
    imagery_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    imagery_instruction_proceed_2.keys = undefined;
    imagery_instruction_proceed_2.rt = undefined;
    _imagery_instruction_proceed_2_allKeys = [];
    psychoJS.experiment.addData('imagery_instruction.started', globalClock.getTime());
    imagery_instructionMaxDuration = null
    // keep track of which components have finished
    imagery_instructionComponents = [];
    imagery_instructionComponents.push(imagery_instruction_title);
    imagery_instructionComponents.push(imagery_instruction_text);
    imagery_instructionComponents.push(imagery_instruction_proceed);
    imagery_instructionComponents.push(imagery_instruction_proceed_2);
    
    for (const thisComponent of imagery_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_instruction' ---
    // get current time
    t = imagery_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_instruction_title* updates
    if (t >= 0.0 && imagery_instruction_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_instruction_title.tStart = t;  // (not accounting for frame time here)
      imagery_instruction_title.frameNStart = frameN;  // exact frame index
      
      imagery_instruction_title.setAutoDraw(true);
    }
    
    
    // *imagery_instruction_text* updates
    if (t >= 0.0 && imagery_instruction_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_instruction_text.tStart = t;  // (not accounting for frame time here)
      imagery_instruction_text.frameNStart = frameN;  // exact frame index
      
      imagery_instruction_text.setAutoDraw(true);
    }
    
    
    // *imagery_instruction_proceed* updates
    if (t >= 0.0 && imagery_instruction_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_instruction_proceed.tStart = t;  // (not accounting for frame time here)
      imagery_instruction_proceed.frameNStart = frameN;  // exact frame index
      
      imagery_instruction_proceed.setAutoDraw(true);
    }
    
    
    // *imagery_instruction_proceed_2* updates
    if (t >= 0.0 && imagery_instruction_proceed_2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_instruction_proceed_2.tStart = t;  // (not accounting for frame time here)
      imagery_instruction_proceed_2.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { imagery_instruction_proceed_2.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { imagery_instruction_proceed_2.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { imagery_instruction_proceed_2.clearEvents(); });
    }
    
    if (imagery_instruction_proceed_2.status === PsychoJS.Status.STARTED) {
      let theseKeys = imagery_instruction_proceed_2.getKeys({keyList: ['space'], waitRelease: false});
      _imagery_instruction_proceed_2_allKeys = _imagery_instruction_proceed_2_allKeys.concat(theseKeys);
      if (_imagery_instruction_proceed_2_allKeys.length > 0) {
        imagery_instruction_proceed_2.keys = _imagery_instruction_proceed_2_allKeys[_imagery_instruction_proceed_2_allKeys.length - 1].name;  // just the last key pressed
        imagery_instruction_proceed_2.rt = _imagery_instruction_proceed_2_allKeys[_imagery_instruction_proceed_2_allKeys.length - 1].rt;
        imagery_instruction_proceed_2.duration = _imagery_instruction_proceed_2_allKeys[_imagery_instruction_proceed_2_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_instruction' ---
    for (const thisComponent of imagery_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_instruction.stopped', globalClock.getTime());
    imagery_instruction_proceed_2.stop();
    // the Routine "imagery_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_task_trialsMaxDurationReached;
var _imagery_task_trials_response_allKeys;
var imagery_task_trialsMaxDuration;
var imagery_task_trialsComponents;
function imagery_task_trialsRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task_trials' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_task_trialsClock.reset();
    routineTimer.reset();
    imagery_task_trialsMaxDurationReached = false;
    // update component parameters for each repeat
    imagery_task_trials_question.setText(task_question);
    imagery_task_trials_word1.setValue(word1);
    imagery_task_trials_word1.secs=2;
    imagery_task_trials_word1.setVolume(8);
    imagery_task_trials_word2.setValue(word2);
    imagery_task_trials_word2.secs=2;
    imagery_task_trials_word2.setVolume(8);
    imagery_task_trials_instruction.setValue(instruction);
    imagery_task_trials_instruction.setVolume(8);
    imagery_task_trials_response.keys = undefined;
    imagery_task_trials_response.rt = undefined;
    _imagery_task_trials_response_allKeys = [];
    imagery_task_trials_order_instruction.setText(order_instruction);
    imagery_task_trials_img1.setImage(img1);
    imagery_task_trials_proceed.setText(task_continue);
    psychoJS.experiment.addData('imagery_task_trials.started', globalClock.getTime());
    imagery_task_trialsMaxDuration = null
    // keep track of which components have finished
    imagery_task_trialsComponents = [];
    imagery_task_trialsComponents.push(imagery_task_trials_question);
    imagery_task_trialsComponents.push(imagery_task_trials_word1);
    imagery_task_trialsComponents.push(imagery_task_trials_word2);
    imagery_task_trialsComponents.push(imagery_task_trials_instruction);
    imagery_task_trialsComponents.push(imagery_task_trials_response);
    imagery_task_trialsComponents.push(imagery_task_trials_order_instruction);
    imagery_task_trialsComponents.push(imagery_task_trials_img1);
    imagery_task_trialsComponents.push(imagery_task_trials_proceed);
    
    for (const thisComponent of imagery_task_trialsComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


var frameRemains;
function imagery_task_trialsRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task_trials' ---
    // get current time
    t = imagery_task_trialsClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_trials_question* updates
    if (t >= 0.0 && imagery_task_trials_question.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_question.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_question.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_question.setAutoDraw(true);
    }
    
    // start/stop imagery_task_trials_word1
    if (t >= 0 && imagery_task_trials_word1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_word1.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_word1.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_trials_word1.play(); });  // screen flip
      imagery_task_trials_word1.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (imagery_task_trials_word1.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= imagery_task_trials_word1.tStart + 0.5) {
        imagery_task_trials_word1.stop();  // stop the sound (if longer than duration)
        imagery_task_trials_word1.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop imagery_task_trials_word2
    if (t >= 2 && imagery_task_trials_word2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_word2.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_word2.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_trials_word2.play(); });  // screen flip
      imagery_task_trials_word2.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (imagery_task_trials_word2.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= imagery_task_trials_word2.tStart + 0.5) {
        imagery_task_trials_word2.stop();  // stop the sound (if longer than duration)
        imagery_task_trials_word2.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop imagery_task_trials_instruction
    if (t >= 4 && imagery_task_trials_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_instruction.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_instruction.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_trials_instruction.play(); });  // screen flip
      imagery_task_trials_instruction.status = PsychoJS.Status.STARTED;
    }
    if (t >= (imagery_task_trials_instruction.getDuration() + imagery_task_trials_instruction.tStart)     && imagery_task_trials_instruction.status === PsychoJS.Status.STARTED) {
      imagery_task_trials_instruction.stop();  // stop the sound (if longer than duration)
      imagery_task_trials_instruction.status = PsychoJS.Status.FINISHED;
    }
    
    // *imagery_task_trials_response* updates
    if (((imagery_task_trials_instruction.status == FINISHED)) && imagery_task_trials_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_response.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { imagery_task_trials_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_trials_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_trials_response.clearEvents(); });
    }
    
    if (imagery_task_trials_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = imagery_task_trials_response.getKeys({keyList: ['1', '2'], waitRelease: false});
      _imagery_task_trials_response_allKeys = _imagery_task_trials_response_allKeys.concat(theseKeys);
      if (_imagery_task_trials_response_allKeys.length > 0) {
        imagery_task_trials_response.keys = _imagery_task_trials_response_allKeys[_imagery_task_trials_response_allKeys.length - 1].name;  // just the last key pressed
        imagery_task_trials_response.rt = _imagery_task_trials_response_allKeys[_imagery_task_trials_response_allKeys.length - 1].rt;
        imagery_task_trials_response.duration = _imagery_task_trials_response_allKeys[_imagery_task_trials_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *imagery_task_trials_order_instruction* updates
    if (t >= 4.5 && imagery_task_trials_order_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_order_instruction.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_order_instruction.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_order_instruction.setAutoDraw(true);
    }
    
    
    // *imagery_task_trials_img1* updates
    if (t >= 4.5 && imagery_task_trials_img1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_img1.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_img1.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_img1.setAutoDraw(true);
    }
    
    
    // *imagery_task_trials_proceed* updates
    if (t >= 4.5 && imagery_task_trials_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_proceed.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_proceed.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_task_trialsComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_task_trialsRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task_trials' ---
    for (const thisComponent of imagery_task_trialsComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task_trials.stopped', globalClock.getTime());
    imagery_task_trials_word1.stop();  // ensure sound has stopped at end of Routine
    imagery_task_trials_word2.stop();  // ensure sound has stopped at end of Routine
    imagery_task_trials_instruction.stop();  // ensure sound has stopped at end of Routine
    imagery_task_trials_response.stop();
    // the Routine "imagery_task_trials" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_task_trials_vividnessMaxDurationReached;
var _imagery_task_trials_vividness_response_allKeys;
var imagery_task_trials_vividnessMaxDuration;
var imagery_task_trials_vividnessComponents;
function imagery_task_trials_vividnessRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task_trials_vividness' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_task_trials_vividnessClock.reset();
    routineTimer.reset();
    imagery_task_trials_vividnessMaxDurationReached = false;
    // update component parameters for each repeat
    imagery_task_trials_vividness_question.setText(task_question);
    imagery_task_trials_vividness_text.setText(task_vividness);
    imagery_task_trials_vividness_response.keys = undefined;
    imagery_task_trials_vividness_response.rt = undefined;
    _imagery_task_trials_vividness_response_allKeys = [];
    psychoJS.experiment.addData('imagery_task_trials_vividness.started', globalClock.getTime());
    imagery_task_trials_vividnessMaxDuration = null
    // keep track of which components have finished
    imagery_task_trials_vividnessComponents = [];
    imagery_task_trials_vividnessComponents.push(imagery_task_trials_vividness_question);
    imagery_task_trials_vividnessComponents.push(imagery_task_trials_vividness_text);
    imagery_task_trials_vividnessComponents.push(imagery_task_trials_vividness_response);
    
    for (const thisComponent of imagery_task_trials_vividnessComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_task_trials_vividnessRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task_trials_vividness' ---
    // get current time
    t = imagery_task_trials_vividnessClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_trials_vividness_question* updates
    if (t >= 0.0 && imagery_task_trials_vividness_question.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_vividness_question.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_vividness_question.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_vividness_question.setAutoDraw(true);
    }
    
    
    // *imagery_task_trials_vividness_text* updates
    if (t >= 0.0 && imagery_task_trials_vividness_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_vividness_text.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_vividness_text.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_vividness_text.setAutoDraw(true);
    }
    
    
    // *imagery_task_trials_vividness_response* updates
    if (t >= 0.0 && imagery_task_trials_vividness_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_vividness_response.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_vividness_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { imagery_task_trials_vividness_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_trials_vividness_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_trials_vividness_response.clearEvents(); });
    }
    
    if (imagery_task_trials_vividness_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = imagery_task_trials_vividness_response.getKeys({keyList: ['1', '2', '3', '4'], waitRelease: false});
      _imagery_task_trials_vividness_response_allKeys = _imagery_task_trials_vividness_response_allKeys.concat(theseKeys);
      if (_imagery_task_trials_vividness_response_allKeys.length > 0) {
        imagery_task_trials_vividness_response.keys = _imagery_task_trials_vividness_response_allKeys[_imagery_task_trials_vividness_response_allKeys.length - 1].name;  // just the last key pressed
        imagery_task_trials_vividness_response.rt = _imagery_task_trials_vividness_response_allKeys[_imagery_task_trials_vividness_response_allKeys.length - 1].rt;
        imagery_task_trials_vividness_response.duration = _imagery_task_trials_vividness_response_allKeys[_imagery_task_trials_vividness_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_task_trials_vividnessComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_task_trials_vividnessRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task_trials_vividness' ---
    for (const thisComponent of imagery_task_trials_vividnessComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task_trials_vividness.stopped', globalClock.getTime());
    imagery_task_trials_vividness_response.stop();
    // the Routine "imagery_task_trials_vividness" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var restingMaxDurationReached;
var restingMaxDuration;
var restingComponents;
function restingRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'resting' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    restingClock.reset(routineTimer.getTime());
    routineTimer.add(0.500000);
    restingMaxDurationReached = false;
    // update component parameters for each repeat
    psychoJS.experiment.addData('resting.started', globalClock.getTime());
    restingMaxDuration = null
    // keep track of which components have finished
    restingComponents = [];
    restingComponents.push(imagery_task_trials_rest);
    
    for (const thisComponent of restingComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function restingRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'resting' ---
    // get current time
    t = restingClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_trials_rest* updates
    if (t >= 0.0 && imagery_task_trials_rest.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_trials_rest.tStart = t;  // (not accounting for frame time here)
      imagery_task_trials_rest.frameNStart = frameN;  // exact frame index
      
      imagery_task_trials_rest.setAutoDraw(true);
    }
    
    frameRemains = 0.0 + 0.5 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (imagery_task_trials_rest.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      imagery_task_trials_rest.setAutoDraw(false);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of restingComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine && routineTimer.getTime() > 0) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function restingRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'resting' ---
    for (const thisComponent of restingComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('resting.stopped', globalClock.getTime());
    if (restingMaxDurationReached) {
        restingClock.add(restingMaxDuration);
    } else {
        restingClock.add(0.500000);
    }
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_task_prepareMaxDurationReached;
var _imagery_task_prepare_response_allKeys;
var imagery_task_prepareMaxDuration;
var imagery_task_prepareComponents;
function imagery_task_prepareRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task_prepare' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_task_prepareClock.reset();
    routineTimer.reset();
    imagery_task_prepareMaxDurationReached = false;
    // update component parameters for each repeat
    imagery_task_prepare_response.keys = undefined;
    imagery_task_prepare_response.rt = undefined;
    _imagery_task_prepare_response_allKeys = [];
    psychoJS.experiment.addData('imagery_task_prepare.started', globalClock.getTime());
    imagery_task_prepareMaxDuration = null
    // keep track of which components have finished
    imagery_task_prepareComponents = [];
    imagery_task_prepareComponents.push(imagery_task_prepare_title);
    imagery_task_prepareComponents.push(imagery_task_prepare_text);
    imagery_task_prepareComponents.push(imagery_task_prepare_response);
    imagery_task_prepareComponents.push(imagery_task_prepare_proceed);
    
    for (const thisComponent of imagery_task_prepareComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_task_prepareRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task_prepare' ---
    // get current time
    t = imagery_task_prepareClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_prepare_title* updates
    if (t >= 0.0 && imagery_task_prepare_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_prepare_title.tStart = t;  // (not accounting for frame time here)
      imagery_task_prepare_title.frameNStart = frameN;  // exact frame index
      
      imagery_task_prepare_title.setAutoDraw(true);
    }
    
    
    // *imagery_task_prepare_text* updates
    if (t >= 0.0 && imagery_task_prepare_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_prepare_text.tStart = t;  // (not accounting for frame time here)
      imagery_task_prepare_text.frameNStart = frameN;  // exact frame index
      
      imagery_task_prepare_text.setAutoDraw(true);
    }
    
    
    // *imagery_task_prepare_response* updates
    if (t >= 0.0 && imagery_task_prepare_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_prepare_response.tStart = t;  // (not accounting for frame time here)
      imagery_task_prepare_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { imagery_task_prepare_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_prepare_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_prepare_response.clearEvents(); });
    }
    
    if (imagery_task_prepare_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = imagery_task_prepare_response.getKeys({keyList: ['space'], waitRelease: false});
      _imagery_task_prepare_response_allKeys = _imagery_task_prepare_response_allKeys.concat(theseKeys);
      if (_imagery_task_prepare_response_allKeys.length > 0) {
        imagery_task_prepare_response.keys = _imagery_task_prepare_response_allKeys[_imagery_task_prepare_response_allKeys.length - 1].name;  // just the last key pressed
        imagery_task_prepare_response.rt = _imagery_task_prepare_response_allKeys[_imagery_task_prepare_response_allKeys.length - 1].rt;
        imagery_task_prepare_response.duration = _imagery_task_prepare_response_allKeys[_imagery_task_prepare_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *imagery_task_prepare_proceed* updates
    if (t >= 0.0 && imagery_task_prepare_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_prepare_proceed.tStart = t;  // (not accounting for frame time here)
      imagery_task_prepare_proceed.frameNStart = frameN;  // exact frame index
      
      imagery_task_prepare_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_task_prepareComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_task_prepareRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task_prepare' ---
    for (const thisComponent of imagery_task_prepareComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task_prepare.stopped', globalClock.getTime());
    imagery_task_prepare_response.stop();
    // the Routine "imagery_task_prepare" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_taskMaxDurationReached;
var _key_resp_vmi_allKeys;
var imagery_taskMaxDuration;
var imagery_taskComponents;
function imagery_taskRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_taskClock.reset();
    routineTimer.reset();
    imagery_taskMaxDurationReached = false;
    // update component parameters for each repeat
    imagery_task_idx.setText(imagery_task_items.thisN);
    imagery_task_word1.setValue(word1);
    imagery_task_word1.secs=2;
    imagery_task_word1.setVolume(8);
    imagery_task_word2.setValue(word2);
    imagery_task_word2.secs=2;
    imagery_task_word2.setVolume(8);
    imagery_task_instruction.setValue(instruction);
    imagery_task_instruction.setVolume(8);
    imagery_task_text.setText('Give your answer: first (key "1") or second (key "2").');
    key_resp_vmi.keys = undefined;
    key_resp_vmi.rt = undefined;
    _key_resp_vmi_allKeys = [];
    psychoJS.experiment.addData('imagery_task.started', globalClock.getTime());
    imagery_taskMaxDuration = null
    // keep track of which components have finished
    imagery_taskComponents = [];
    imagery_taskComponents.push(imagery_task_title);
    imagery_taskComponents.push(imagery_task_idx);
    imagery_taskComponents.push(imagery_task_word1);
    imagery_taskComponents.push(imagery_task_word2);
    imagery_taskComponents.push(imagery_task_instruction);
    imagery_taskComponents.push(imagery_task_text);
    imagery_taskComponents.push(key_resp_vmi);
    
    for (const thisComponent of imagery_taskComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_taskRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task' ---
    // get current time
    t = imagery_taskClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_title* updates
    if (t >= 0.0 && imagery_task_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_title.tStart = t;  // (not accounting for frame time here)
      imagery_task_title.frameNStart = frameN;  // exact frame index
      
      imagery_task_title.setAutoDraw(true);
    }
    
    
    // *imagery_task_idx* updates
    if (t >= 0.0 && imagery_task_idx.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_idx.tStart = t;  // (not accounting for frame time here)
      imagery_task_idx.frameNStart = frameN;  // exact frame index
      
      imagery_task_idx.setAutoDraw(true);
    }
    
    // start/stop imagery_task_word1
    if (t >= 0 && imagery_task_word1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_word1.tStart = t;  // (not accounting for frame time here)
      imagery_task_word1.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_word1.play(); });  // screen flip
      imagery_task_word1.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (imagery_task_word1.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= imagery_task_word1.tStart + 0.5) {
        imagery_task_word1.stop();  // stop the sound (if longer than duration)
        imagery_task_word1.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop imagery_task_word2
    if (t >= 2 && imagery_task_word2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_word2.tStart = t;  // (not accounting for frame time here)
      imagery_task_word2.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_word2.play(); });  // screen flip
      imagery_task_word2.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (imagery_task_word2.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= imagery_task_word2.tStart + 0.5) {
        imagery_task_word2.stop();  // stop the sound (if longer than duration)
        imagery_task_word2.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop imagery_task_instruction
    if (t >= 4 && imagery_task_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_instruction.tStart = t;  // (not accounting for frame time here)
      imagery_task_instruction.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ imagery_task_instruction.play(); });  // screen flip
      imagery_task_instruction.status = PsychoJS.Status.STARTED;
    }
    if (t >= (imagery_task_instruction.getDuration() + imagery_task_instruction.tStart)     && imagery_task_instruction.status === PsychoJS.Status.STARTED) {
      imagery_task_instruction.stop();  // stop the sound (if longer than duration)
      imagery_task_instruction.status = PsychoJS.Status.FINISHED;
    }
    
    // *imagery_task_text* updates
    if (t >= 0 && imagery_task_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_text.tStart = t;  // (not accounting for frame time here)
      imagery_task_text.frameNStart = frameN;  // exact frame index
      
      imagery_task_text.setAutoDraw(true);
    }
    
    
    // *key_resp_vmi* updates
    if (((imagery_task_instruction.status == FINISHED)) && key_resp_vmi.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_resp_vmi.tStart = t;  // (not accounting for frame time here)
      key_resp_vmi.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { key_resp_vmi.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vmi.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vmi.clearEvents(); });
    }
    
    if (key_resp_vmi.status === PsychoJS.Status.STARTED) {
      let theseKeys = key_resp_vmi.getKeys({keyList: ['1', '2'], waitRelease: false});
      _key_resp_vmi_allKeys = _key_resp_vmi_allKeys.concat(theseKeys);
      if (_key_resp_vmi_allKeys.length > 0) {
        key_resp_vmi.keys = _key_resp_vmi_allKeys[_key_resp_vmi_allKeys.length - 1].name;  // just the last key pressed
        key_resp_vmi.rt = _key_resp_vmi_allKeys[_key_resp_vmi_allKeys.length - 1].rt;
        key_resp_vmi.duration = _key_resp_vmi_allKeys[_key_resp_vmi_allKeys.length - 1].duration;
        // was this correct?
        if (key_resp_vmi.keys == correctAns) {
            key_resp_vmi.corr = 1;
        } else {
            key_resp_vmi.corr = 0;
        }
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_taskComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_taskRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task' ---
    for (const thisComponent of imagery_taskComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task.stopped', globalClock.getTime());
    imagery_task_word1.stop();  // ensure sound has stopped at end of Routine
    imagery_task_word2.stop();  // ensure sound has stopped at end of Routine
    imagery_task_instruction.stop();  // ensure sound has stopped at end of Routine
    // was no response the correct answer?!
    if (key_resp_vmi.keys === undefined) {
      if (['None','none',undefined].includes(correctAns)) {
         key_resp_vmi.corr = 1;  // correct non-response
      } else {
         key_resp_vmi.corr = 0;  // failed to respond (incorrectly)
      }
    }
    // store data for current loop
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(key_resp_vmi.corr, level);
    }
    psychoJS.experiment.addData('key_resp_vmi.keys', key_resp_vmi.keys);
    psychoJS.experiment.addData('key_resp_vmi.corr', key_resp_vmi.corr);
    if (typeof key_resp_vmi.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('key_resp_vmi.rt', key_resp_vmi.rt);
        psychoJS.experiment.addData('key_resp_vmi.duration', key_resp_vmi.duration);
        routineTimer.reset();
        }
    
    key_resp_vmi.stop();
    // the Routine "imagery_task" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_task_vividnessMaxDurationReached;
var _vividness_allKeys;
var imagery_task_vividnessMaxDuration;
var imagery_task_vividnessComponents;
function imagery_task_vividnessRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task_vividness' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_task_vividnessClock.reset();
    routineTimer.reset();
    imagery_task_vividnessMaxDurationReached = false;
    // update component parameters for each repeat
    vividness.keys = undefined;
    vividness.rt = undefined;
    _vividness_allKeys = [];
    psychoJS.experiment.addData('imagery_task_vividness.started', globalClock.getTime());
    imagery_task_vividnessMaxDuration = null
    // keep track of which components have finished
    imagery_task_vividnessComponents = [];
    imagery_task_vividnessComponents.push(imagery_task_vividness_title);
    imagery_task_vividnessComponents.push(vividness);
    
    for (const thisComponent of imagery_task_vividnessComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_task_vividnessRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task_vividness' ---
    // get current time
    t = imagery_task_vividnessClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_vividness_title* updates
    if (t >= 0.0 && imagery_task_vividness_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_vividness_title.tStart = t;  // (not accounting for frame time here)
      imagery_task_vividness_title.frameNStart = frameN;  // exact frame index
      
      imagery_task_vividness_title.setAutoDraw(true);
    }
    
    
    // *vividness* updates
    if (t >= 0.0 && vividness.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      vividness.tStart = t;  // (not accounting for frame time here)
      vividness.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { vividness.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { vividness.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { vividness.clearEvents(); });
    }
    
    if (vividness.status === PsychoJS.Status.STARTED) {
      let theseKeys = vividness.getKeys({keyList: ['1', '2', '3', '4'], waitRelease: false});
      _vividness_allKeys = _vividness_allKeys.concat(theseKeys);
      if (_vividness_allKeys.length > 0) {
        vividness.keys = _vividness_allKeys[_vividness_allKeys.length - 1].name;  // just the last key pressed
        vividness.rt = _vividness_allKeys[_vividness_allKeys.length - 1].rt;
        vividness.duration = _vividness_allKeys[_vividness_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_task_vividnessComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_task_vividnessRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task_vividness' ---
    for (const thisComponent of imagery_task_vividnessComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task_vividness.stopped', globalClock.getTime());
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(vividness.corr, level);
    }
    psychoJS.experiment.addData('vividness.keys', vividness.keys);
    if (typeof vividness.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('vividness.rt', vividness.rt);
        psychoJS.experiment.addData('vividness.duration', vividness.duration);
        routineTimer.reset();
        }
    
    vividness.stop();
    // the Routine "imagery_task_vividness" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var imagery_task_feedbackMaxDurationReached;
var corrRate;
var _imagery_task_feedback_response_allKeys;
var imagery_task_feedbackMaxDuration;
var imagery_task_feedbackComponents;
function imagery_task_feedbackRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'imagery_task_feedback' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    imagery_task_feedbackClock.reset();
    routineTimer.reset();
    imagery_task_feedbackMaxDurationReached = false;
    // update component parameters for each repeat
    // Run 'Begin Routine' code from imagery_task_feedback_text
    responses = psychoJS.experiment._trialsData;
    subBlock = responses.slice(previousLength + 1);
    console.log("subBlock est : ");
    console.log(subBlock);
    
    let meanRT = 0;
    subBlock.forEach(entry => {
        let value = parseFloat(entry["key_resp_vmi.rt"]);
        // Check if the value is a number
        if (!isNaN(value)) {
            meanRT += value;
        } else {
            console.log('Invalid RT value:', value);
        }
    });
    meanRT /= subBlock.length;
    
    let corrRate = 0;
    subBlock.forEach(entry => {
        let respKeys = entry["key_resp_vmi.keys"];
        let correctAns = entry["correctAns"] ? entry["correctAns"].toString() : null;
        if (respKeys == correctAns) {
            corrRate += 1;
            entry["key_resp_vmi.corr"] = 1;
        } else {
            entry["key_resp_vmi.corr"] = 0;
        }
    });
    corrRate = corrRate / subBlock.length * 100;
    
    console.log("meanRt est : ");
    console.log(meanRT);
    
    if (corrRate > 70) {
        msg = "You answered "+ corrRate.toFixed(0) + " percent of trials correctly with an average response time of " + meanRT.toFixed(1) + " seconds. Bravo! Keep going!" ;
        colortext = 'green';
    } else {
        msg = "You answered "+ corrRate.toFixed(0) + " percent of trials correctly with an average response time of " + meanRT.toFixed(1) + " seconds. Did you have difficulty imagining the items for this category? Keep going!";
        colortext = 'red';
    }
    
    previousLength = responses.length;
    
    imagery_task_feedback_response.keys = undefined;
    imagery_task_feedback_response.rt = undefined;
    _imagery_task_feedback_response_allKeys = [];
    psychoJS.experiment.addData('imagery_task_feedback.started', globalClock.getTime());
    imagery_task_feedbackMaxDuration = null
    // keep track of which components have finished
    imagery_task_feedbackComponents = [];
    imagery_task_feedbackComponents.push(imagery_task_feedback_title);
    imagery_task_feedbackComponents.push(imagery_task_feedback_text_1);
    imagery_task_feedbackComponents.push(imagery_task_feedback_response);
    imagery_task_feedbackComponents.push(imagery_task_feedback_proceed);
    
    for (const thisComponent of imagery_task_feedbackComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function imagery_task_feedbackRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'imagery_task_feedback' ---
    // get current time
    t = imagery_task_feedbackClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *imagery_task_feedback_title* updates
    if (t >= 0.0 && imagery_task_feedback_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_feedback_title.tStart = t;  // (not accounting for frame time here)
      imagery_task_feedback_title.frameNStart = frameN;  // exact frame index
      
      imagery_task_feedback_title.setAutoDraw(true);
    }
    
    
    if (imagery_task_feedback_text_1.status === PsychoJS.Status.STARTED){ // only update if being drawn
      imagery_task_feedback_text_1.setColor(new util.Color(colortext), false);
      imagery_task_feedback_text_1.setText(msg, false);
    }
    
    // *imagery_task_feedback_text_1* updates
    if (t >= 0.0 && imagery_task_feedback_text_1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_feedback_text_1.tStart = t;  // (not accounting for frame time here)
      imagery_task_feedback_text_1.frameNStart = frameN;  // exact frame index
      
      imagery_task_feedback_text_1.setAutoDraw(true);
    }
    
    
    // *imagery_task_feedback_response* updates
    if (t >= 0.0 && imagery_task_feedback_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_feedback_response.tStart = t;  // (not accounting for frame time here)
      imagery_task_feedback_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { imagery_task_feedback_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_feedback_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { imagery_task_feedback_response.clearEvents(); });
    }
    
    if (imagery_task_feedback_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = imagery_task_feedback_response.getKeys({keyList: ['space'], waitRelease: false});
      _imagery_task_feedback_response_allKeys = _imagery_task_feedback_response_allKeys.concat(theseKeys);
      if (_imagery_task_feedback_response_allKeys.length > 0) {
        imagery_task_feedback_response.keys = _imagery_task_feedback_response_allKeys[_imagery_task_feedback_response_allKeys.length - 1].name;  // just the last key pressed
        imagery_task_feedback_response.rt = _imagery_task_feedback_response_allKeys[_imagery_task_feedback_response_allKeys.length - 1].rt;
        imagery_task_feedback_response.duration = _imagery_task_feedback_response_allKeys[_imagery_task_feedback_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *imagery_task_feedback_proceed* updates
    if (t >= 0.0 && imagery_task_feedback_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      imagery_task_feedback_proceed.tStart = t;  // (not accounting for frame time here)
      imagery_task_feedback_proceed.frameNStart = frameN;  // exact frame index
      
      imagery_task_feedback_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of imagery_task_feedbackComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function imagery_task_feedbackRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'imagery_task_feedback' ---
    for (const thisComponent of imagery_task_feedbackComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('imagery_task_feedback.stopped', globalClock.getTime());
    imagery_task_feedback_response.stop();
    // the Routine "imagery_task_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var perception_instructionMaxDurationReached;
var _perception_instruction_response_allKeys;
var perception_instructionMaxDuration;
var perception_instructionComponents;
function perception_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'perception_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    perception_instructionClock.reset();
    routineTimer.reset();
    perception_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    perception_instruction_response.keys = undefined;
    perception_instruction_response.rt = undefined;
    _perception_instruction_response_allKeys = [];
    psychoJS.experiment.addData('perception_instruction.started', globalClock.getTime());
    perception_instructionMaxDuration = null
    // keep track of which components have finished
    perception_instructionComponents = [];
    perception_instructionComponents.push(perception_instruction_title);
    perception_instructionComponents.push(perception_instruction_task);
    perception_instructionComponents.push(perception_instruction_response);
    perception_instructionComponents.push(perception_instruction_proceed);
    
    for (const thisComponent of perception_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function perception_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'perception_instruction' ---
    // get current time
    t = perception_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *perception_instruction_title* updates
    if (t >= 0.0 && perception_instruction_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_instruction_title.tStart = t;  // (not accounting for frame time here)
      perception_instruction_title.frameNStart = frameN;  // exact frame index
      
      perception_instruction_title.setAutoDraw(true);
    }
    
    
    // *perception_instruction_task* updates
    if (t >= 0.0 && perception_instruction_task.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_instruction_task.tStart = t;  // (not accounting for frame time here)
      perception_instruction_task.frameNStart = frameN;  // exact frame index
      
      perception_instruction_task.setAutoDraw(true);
    }
    
    
    // *perception_instruction_response* updates
    if (t >= 0.0 && perception_instruction_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_instruction_response.tStart = t;  // (not accounting for frame time here)
      perception_instruction_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { perception_instruction_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { perception_instruction_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { perception_instruction_response.clearEvents(); });
    }
    
    if (perception_instruction_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = perception_instruction_response.getKeys({keyList: ['space'], waitRelease: false});
      _perception_instruction_response_allKeys = _perception_instruction_response_allKeys.concat(theseKeys);
      if (_perception_instruction_response_allKeys.length > 0) {
        perception_instruction_response.keys = _perception_instruction_response_allKeys[_perception_instruction_response_allKeys.length - 1].name;  // just the last key pressed
        perception_instruction_response.rt = _perception_instruction_response_allKeys[_perception_instruction_response_allKeys.length - 1].rt;
        perception_instruction_response.duration = _perception_instruction_response_allKeys[_perception_instruction_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *perception_instruction_proceed* updates
    if (t >= 0.0 && perception_instruction_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_instruction_proceed.tStart = t;  // (not accounting for frame time here)
      perception_instruction_proceed.frameNStart = frameN;  // exact frame index
      
      perception_instruction_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of perception_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function perception_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'perception_instruction' ---
    for (const thisComponent of perception_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('perception_instruction.stopped', globalClock.getTime());
    perception_instruction_response.stop();
    // the Routine "perception_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var perception_taskMaxDurationReached;
var _perception_allKeys;
var perception_taskMaxDuration;
var perception_taskComponents;
function perception_taskRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'perception_task' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    perception_taskClock.reset();
    routineTimer.reset();
    perception_taskMaxDurationReached = false;
    // update component parameters for each repeat
    perception_task_word1.setValue(word1);
    perception_task_word1.secs=2;
    perception_task_word1.setVolume(8);
    perception_task_img1.setImage(img1);
    perception_task_word2.setValue(word2);
    perception_task_word2.secs=2;
    perception_task_word2.setVolume(8);
    perception_task_img2.setImage(img2);
    perception_task_instruction.setValue(instruction);
    perception_task_instruction.setVolume(8);
    perception_task_img3.setImage(imgCompare);
    perception.keys = undefined;
    perception.rt = undefined;
    _perception_allKeys = [];
    perception_task_text.setText('Give your answer: first (key "1") or second (key "2").');
    psychoJS.experiment.addData('perception_task.started', globalClock.getTime());
    perception_taskMaxDuration = null
    // keep track of which components have finished
    perception_taskComponents = [];
    perception_taskComponents.push(perception_task_title);
    perception_taskComponents.push(perception_task_word1);
    perception_taskComponents.push(perception_task_img1);
    perception_taskComponents.push(perception_task_word2);
    perception_taskComponents.push(perception_task_img2);
    perception_taskComponents.push(perception_task_instruction);
    perception_taskComponents.push(perception_task_img3);
    perception_taskComponents.push(perception);
    perception_taskComponents.push(perception_task_text);
    
    for (const thisComponent of perception_taskComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function perception_taskRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'perception_task' ---
    // get current time
    t = perception_taskClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *perception_task_title* updates
    if (t >= 0.0 && perception_task_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_title.tStart = t;  // (not accounting for frame time here)
      perception_task_title.frameNStart = frameN;  // exact frame index
      
      perception_task_title.setAutoDraw(true);
    }
    
    // start/stop perception_task_word1
    if (t >= 0 && perception_task_word1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_word1.tStart = t;  // (not accounting for frame time here)
      perception_task_word1.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ perception_task_word1.play(); });  // screen flip
      perception_task_word1.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (perception_task_word1.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= perception_task_word1.tStart + 0.5) {
        perception_task_word1.stop();  // stop the sound (if longer than duration)
        perception_task_word1.status = PsychoJS.Status.FINISHED;
      }
    }
    
    // *perception_task_img1* updates
    if (t >= 0 && perception_task_img1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_img1.tStart = t;  // (not accounting for frame time here)
      perception_task_img1.frameNStart = frameN;  // exact frame index
      
      perception_task_img1.setAutoDraw(true);
    }
    
    frameRemains = 0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (perception_task_img1.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      perception_task_img1.setAutoDraw(false);
    }
    
    // start/stop perception_task_word2
    if (t >= 2 && perception_task_word2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_word2.tStart = t;  // (not accounting for frame time here)
      perception_task_word2.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ perception_task_word2.play(); });  // screen flip
      perception_task_word2.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (perception_task_word2.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= perception_task_word2.tStart + 0.5) {
        perception_task_word2.stop();  // stop the sound (if longer than duration)
        perception_task_word2.status = PsychoJS.Status.FINISHED;
      }
    }
    
    // *perception_task_img2* updates
    if (t >= 2 && perception_task_img2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_img2.tStart = t;  // (not accounting for frame time here)
      perception_task_img2.frameNStart = frameN;  // exact frame index
      
      perception_task_img2.setAutoDraw(true);
    }
    
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (perception_task_img2.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      perception_task_img2.setAutoDraw(false);
    }
    
    // start/stop perception_task_instruction
    if (t >= 4 && perception_task_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_instruction.tStart = t;  // (not accounting for frame time here)
      perception_task_instruction.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ perception_task_instruction.play(); });  // screen flip
      perception_task_instruction.status = PsychoJS.Status.STARTED;
    }
    if (t >= (perception_task_instruction.getDuration() + perception_task_instruction.tStart)     && perception_task_instruction.status === PsychoJS.Status.STARTED) {
      perception_task_instruction.stop();  // stop the sound (if longer than duration)
      perception_task_instruction.status = PsychoJS.Status.FINISHED;
    }
    
    // *perception_task_img3* updates
    if (t >= 4 && perception_task_img3.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_img3.tStart = t;  // (not accounting for frame time here)
      perception_task_img3.frameNStart = frameN;  // exact frame index
      
      perception_task_img3.setAutoDraw(true);
    }
    
    
    // *perception* updates
    if (((perception_task_instruction.status == FINISHED)) && perception.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception.tStart = t;  // (not accounting for frame time here)
      perception.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { perception.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { perception.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { perception.clearEvents(); });
    }
    
    if (perception.status === PsychoJS.Status.STARTED) {
      let theseKeys = perception.getKeys({keyList: ['1', '2'], waitRelease: false});
      _perception_allKeys = _perception_allKeys.concat(theseKeys);
      if (_perception_allKeys.length > 0) {
        perception.keys = _perception_allKeys[_perception_allKeys.length - 1].name;  // just the last key pressed
        perception.rt = _perception_allKeys[_perception_allKeys.length - 1].rt;
        perception.duration = _perception_allKeys[_perception_allKeys.length - 1].duration;
        // was this correct?
        if (perception.keys == correctAns) {
            perception.corr = 1;
        } else {
            perception.corr = 0;
        }
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *perception_task_text* updates
    if (t >= 0 && perception_task_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_text.tStart = t;  // (not accounting for frame time here)
      perception_task_text.frameNStart = frameN;  // exact frame index
      
      perception_task_text.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of perception_taskComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function perception_taskRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'perception_task' ---
    for (const thisComponent of perception_taskComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('perception_task.stopped', globalClock.getTime());
    perception_task_word1.stop();  // ensure sound has stopped at end of Routine
    perception_task_word2.stop();  // ensure sound has stopped at end of Routine
    perception_task_instruction.stop();  // ensure sound has stopped at end of Routine
    // was no response the correct answer?!
    if (perception.keys === undefined) {
      if (['None','none',undefined].includes(correctAns)) {
         perception.corr = 1;  // correct non-response
      } else {
         perception.corr = 0;  // failed to respond (incorrectly)
      }
    }
    // store data for current loop
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(perception.corr, level);
    }
    psychoJS.experiment.addData('perception.keys', perception.keys);
    psychoJS.experiment.addData('perception.corr', perception.corr);
    if (typeof perception.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('perception.rt', perception.rt);
        psychoJS.experiment.addData('perception.duration', perception.duration);
        routineTimer.reset();
        }
    
    perception.stop();
    // the Routine "perception_task" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var perception_task_confidenceMaxDurationReached;
var _confidence_allKeys;
var perception_task_confidenceMaxDuration;
var perception_task_confidenceComponents;
function perception_task_confidenceRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'perception_task_confidence' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    perception_task_confidenceClock.reset();
    routineTimer.reset();
    perception_task_confidenceMaxDurationReached = false;
    // update component parameters for each repeat
    perception_task_confidence_idx.setText(perception_task_trial.thisN);
    confidence.keys = undefined;
    confidence.rt = undefined;
    _confidence_allKeys = [];
    psychoJS.experiment.addData('perception_task_confidence.started', globalClock.getTime());
    perception_task_confidenceMaxDuration = null
    // keep track of which components have finished
    perception_task_confidenceComponents = [];
    perception_task_confidenceComponents.push(perception_task_confidence_idx);
    perception_task_confidenceComponents.push(perception_task_confidence_text);
    perception_task_confidenceComponents.push(confidence);
    
    for (const thisComponent of perception_task_confidenceComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function perception_task_confidenceRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'perception_task_confidence' ---
    // get current time
    t = perception_task_confidenceClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *perception_task_confidence_idx* updates
    if (t >= 0.0 && perception_task_confidence_idx.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_confidence_idx.tStart = t;  // (not accounting for frame time here)
      perception_task_confidence_idx.frameNStart = frameN;  // exact frame index
      
      perception_task_confidence_idx.setAutoDraw(true);
    }
    
    
    // *perception_task_confidence_text* updates
    if (t >= 0.0 && perception_task_confidence_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_confidence_text.tStart = t;  // (not accounting for frame time here)
      perception_task_confidence_text.frameNStart = frameN;  // exact frame index
      
      perception_task_confidence_text.setAutoDraw(true);
    }
    
    
    // *confidence* updates
    if (t >= 0.0 && confidence.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      confidence.tStart = t;  // (not accounting for frame time here)
      confidence.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { confidence.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { confidence.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { confidence.clearEvents(); });
    }
    
    if (confidence.status === PsychoJS.Status.STARTED) {
      let theseKeys = confidence.getKeys({keyList: ['1', '2', '3', '4'], waitRelease: false});
      _confidence_allKeys = _confidence_allKeys.concat(theseKeys);
      if (_confidence_allKeys.length > 0) {
        confidence.keys = _confidence_allKeys[_confidence_allKeys.length - 1].name;  // just the last key pressed
        confidence.rt = _confidence_allKeys[_confidence_allKeys.length - 1].rt;
        confidence.duration = _confidence_allKeys[_confidence_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of perception_task_confidenceComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function perception_task_confidenceRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'perception_task_confidence' ---
    for (const thisComponent of perception_task_confidenceComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('perception_task_confidence.stopped', globalClock.getTime());
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(confidence.corr, level);
    }
    psychoJS.experiment.addData('confidence.keys', confidence.keys);
    if (typeof confidence.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('confidence.rt', confidence.rt);
        psychoJS.experiment.addData('confidence.duration', confidence.duration);
        routineTimer.reset();
        }
    
    confidence.stop();
    // the Routine "perception_task_confidence" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var perception_task_feedbackMaxDurationReached;
var _perception_task_feedback_response_allKeys;
var perception_task_feedbackMaxDuration;
var perception_task_feedbackComponents;
function perception_task_feedbackRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'perception_task_feedback' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    perception_task_feedbackClock.reset();
    routineTimer.reset();
    perception_task_feedbackMaxDurationReached = false;
    // update component parameters for each repeat
    // Run 'Begin Routine' code from perception_task_feedback_text
    
    responses = psychoJS.experiment._trialsData;
    subBlock = responses.slice(previousLength + 1);
    console.log("subBlock est : ");
    console.log(subBlock);
    
    let meanRT = 0;
    subBlock.forEach(entry => {
        let value = parseFloat(entry["perception.rt"]);
        // Check if the value is a number
        if (!isNaN(value)) {
            meanRT += value;
        } else {
            console.log('Invalid RT value:', value);
        }
    });
    meanRT /= subBlock.length;
    
    let corrRate = 0;
    subBlock.forEach(entry => {
        let respKeys = entry["perception.keys"];
        let correctAns = entry["correctAns"] ? entry["correctAns"].toString() : null;
        if (respKeys == correctAns) {
            corrRate += 1;
            entry["perception.corr"] = 1;
        } else {
            entry["perception.corr"] = 0;
        }
    });
    corrRate = corrRate / subBlock.length * 100;
    
    console.log("meanRt est : ");
    console.log(meanRT);
    
    if (corrRate > 75) {
        msg = "You answered "+ corrRate.toFixed(0) + " percent of the trials correctly with an average response time of " + meanRT.toFixed(1) + " seconds. Bravo!" ;
        colortext = 'green';
    } else {
        msg = "You answered "+ corrRate.toFixed(0) + " percent of the trials correctly with an average response time of " + meanRT.toFixed(1) + " seconds. Did you encounter any difficulties?" ;
        colortext = 'red';
    }
    
    previousLength = responses.length;
    
    perception_task_feedback_response.keys = undefined;
    perception_task_feedback_response.rt = undefined;
    _perception_task_feedback_response_allKeys = [];
    psychoJS.experiment.addData('perception_task_feedback.started', globalClock.getTime());
    perception_task_feedbackMaxDuration = null
    // keep track of which components have finished
    perception_task_feedbackComponents = [];
    perception_task_feedbackComponents.push(perception_task_feedback_title);
    perception_task_feedbackComponents.push(perception_task_feedback_text_1);
    perception_task_feedbackComponents.push(perception_task_feedback_response);
    perception_task_feedbackComponents.push(perception_task_feedback_proceed);
    
    for (const thisComponent of perception_task_feedbackComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function perception_task_feedbackRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'perception_task_feedback' ---
    // get current time
    t = perception_task_feedbackClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *perception_task_feedback_title* updates
    if (t >= 0.0 && perception_task_feedback_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_feedback_title.tStart = t;  // (not accounting for frame time here)
      perception_task_feedback_title.frameNStart = frameN;  // exact frame index
      
      perception_task_feedback_title.setAutoDraw(true);
    }
    
    
    if (perception_task_feedback_text_1.status === PsychoJS.Status.STARTED){ // only update if being drawn
      perception_task_feedback_text_1.setColor(new util.Color(colortext), false);
      perception_task_feedback_text_1.setText(msg, false);
    }
    
    // *perception_task_feedback_text_1* updates
    if (t >= 0.0 && perception_task_feedback_text_1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_feedback_text_1.tStart = t;  // (not accounting for frame time here)
      perception_task_feedback_text_1.frameNStart = frameN;  // exact frame index
      
      perception_task_feedback_text_1.setAutoDraw(true);
    }
    
    
    // *perception_task_feedback_response* updates
    if (t >= 0.0 && perception_task_feedback_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_feedback_response.tStart = t;  // (not accounting for frame time here)
      perception_task_feedback_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { perception_task_feedback_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { perception_task_feedback_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { perception_task_feedback_response.clearEvents(); });
    }
    
    if (perception_task_feedback_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = perception_task_feedback_response.getKeys({keyList: ['space'], waitRelease: false});
      _perception_task_feedback_response_allKeys = _perception_task_feedback_response_allKeys.concat(theseKeys);
      if (_perception_task_feedback_response_allKeys.length > 0) {
        perception_task_feedback_response.keys = _perception_task_feedback_response_allKeys[_perception_task_feedback_response_allKeys.length - 1].name;  // just the last key pressed
        perception_task_feedback_response.rt = _perception_task_feedback_response_allKeys[_perception_task_feedback_response_allKeys.length - 1].rt;
        perception_task_feedback_response.duration = _perception_task_feedback_response_allKeys[_perception_task_feedback_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *perception_task_feedback_proceed* updates
    if (t >= 0.0 && perception_task_feedback_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      perception_task_feedback_proceed.tStart = t;  // (not accounting for frame time here)
      perception_task_feedback_proceed.frameNStart = frameN;  // exact frame index
      
      perception_task_feedback_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of perception_task_feedbackComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function perception_task_feedbackRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'perception_task_feedback' ---
    for (const thisComponent of perception_task_feedbackComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('perception_task_feedback.stopped', globalClock.getTime());
    perception_task_feedback_response.stop();
    // the Routine "perception_task_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var sem_instructionMaxDurationReached;
var _semantics_instruction_response_allKeys;
var sem_instructionMaxDuration;
var sem_instructionComponents;
function sem_instructionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'sem_instruction' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    sem_instructionClock.reset();
    routineTimer.reset();
    sem_instructionMaxDurationReached = false;
    // update component parameters for each repeat
    semantics_instruction_response.keys = undefined;
    semantics_instruction_response.rt = undefined;
    _semantics_instruction_response_allKeys = [];
    psychoJS.experiment.addData('sem_instruction.started', globalClock.getTime());
    sem_instructionMaxDuration = null
    // keep track of which components have finished
    sem_instructionComponents = [];
    sem_instructionComponents.push(semantics_instruction_title);
    sem_instructionComponents.push(semantics_instruction_task);
    sem_instructionComponents.push(semantics_instruction_response);
    sem_instructionComponents.push(semantics_instruction_proceed);
    
    for (const thisComponent of sem_instructionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function sem_instructionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'sem_instruction' ---
    // get current time
    t = sem_instructionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *semantics_instruction_title* updates
    if (t >= 0.0 && semantics_instruction_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_instruction_title.tStart = t;  // (not accounting for frame time here)
      semantics_instruction_title.frameNStart = frameN;  // exact frame index
      
      semantics_instruction_title.setAutoDraw(true);
    }
    
    
    // *semantics_instruction_task* updates
    if (t >= 0.0 && semantics_instruction_task.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_instruction_task.tStart = t;  // (not accounting for frame time here)
      semantics_instruction_task.frameNStart = frameN;  // exact frame index
      
      semantics_instruction_task.setAutoDraw(true);
    }
    
    
    // *semantics_instruction_response* updates
    if (t >= 0.0 && semantics_instruction_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_instruction_response.tStart = t;  // (not accounting for frame time here)
      semantics_instruction_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { semantics_instruction_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { semantics_instruction_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { semantics_instruction_response.clearEvents(); });
    }
    
    if (semantics_instruction_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = semantics_instruction_response.getKeys({keyList: ['space'], waitRelease: false});
      _semantics_instruction_response_allKeys = _semantics_instruction_response_allKeys.concat(theseKeys);
      if (_semantics_instruction_response_allKeys.length > 0) {
        semantics_instruction_response.keys = _semantics_instruction_response_allKeys[_semantics_instruction_response_allKeys.length - 1].name;  // just the last key pressed
        semantics_instruction_response.rt = _semantics_instruction_response_allKeys[_semantics_instruction_response_allKeys.length - 1].rt;
        semantics_instruction_response.duration = _semantics_instruction_response_allKeys[_semantics_instruction_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *semantics_instruction_proceed* updates
    if (t >= 0.0 && semantics_instruction_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_instruction_proceed.tStart = t;  // (not accounting for frame time here)
      semantics_instruction_proceed.frameNStart = frameN;  // exact frame index
      
      semantics_instruction_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of sem_instructionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function sem_instructionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'sem_instruction' ---
    for (const thisComponent of sem_instructionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('sem_instruction.stopped', globalClock.getTime());
    semantics_instruction_response.stop();
    // the Routine "sem_instruction" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var semantics_task_trialsMaxDurationReached;
var _semantics_task_trials_response_allKeys;
var semantics_task_trialsMaxDuration;
var semantics_task_trialsComponents;
function semantics_task_trialsRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'semantics_task_trials' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    semantics_task_trialsClock.reset();
    routineTimer.reset();
    semantics_task_trialsMaxDurationReached = false;
    // update component parameters for each repeat
    semantics_task_trials_question.setText(task_question);
    semantics_task_trials_word1.setValue(word1);
    semantics_task_trials_word1.secs=2;
    semantics_task_trials_word1.setVolume(8);
    semantics_task_trials_word2.setValue(word2);
    semantics_task_trials_word2.secs=2;
    semantics_task_trials_word2.setVolume(8);
    semantics_task_trials_instruction.setValue(instruction);
    semantics_task_trials_instruction.setVolume(8);
    semantics_task_trials_response.keys = undefined;
    semantics_task_trials_response.rt = undefined;
    _semantics_task_trials_response_allKeys = [];
    semantics_task_trials_order_instruction.setText(order_instruction);
    semantics_task_trials_proceed.setText(task_continue);
    psychoJS.experiment.addData('semantics_task_trials.started', globalClock.getTime());
    semantics_task_trialsMaxDuration = null
    // keep track of which components have finished
    semantics_task_trialsComponents = [];
    semantics_task_trialsComponents.push(semantics_task_trials_question);
    semantics_task_trialsComponents.push(semantics_task_trials_word1);
    semantics_task_trialsComponents.push(semantics_task_trials_word2);
    semantics_task_trialsComponents.push(semantics_task_trials_instruction);
    semantics_task_trialsComponents.push(semantics_task_trials_response);
    semantics_task_trialsComponents.push(semantics_task_trials_order_instruction);
    semantics_task_trialsComponents.push(semantics_task_trials_proceed);
    
    for (const thisComponent of semantics_task_trialsComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function semantics_task_trialsRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'semantics_task_trials' ---
    // get current time
    t = semantics_task_trialsClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *semantics_task_trials_question* updates
    if (t >= 0.0 && semantics_task_trials_question.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_question.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_question.frameNStart = frameN;  // exact frame index
      
      semantics_task_trials_question.setAutoDraw(true);
    }
    
    // start/stop semantics_task_trials_word1
    if (t >= 0 && semantics_task_trials_word1.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_word1.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_word1.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ semantics_task_trials_word1.play(); });  // screen flip
      semantics_task_trials_word1.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (semantics_task_trials_word1.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= semantics_task_trials_word1.tStart + 0.5) {
        semantics_task_trials_word1.stop();  // stop the sound (if longer than duration)
        semantics_task_trials_word1.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop semantics_task_trials_word2
    if (t >= 2 && semantics_task_trials_word2.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_word2.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_word2.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ semantics_task_trials_word2.play(); });  // screen flip
      semantics_task_trials_word2.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (semantics_task_trials_word2.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= semantics_task_trials_word2.tStart + 0.5) {
        semantics_task_trials_word2.stop();  // stop the sound (if longer than duration)
        semantics_task_trials_word2.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop semantics_task_trials_instruction
    if (t >= 4 && semantics_task_trials_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_instruction.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_instruction.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ semantics_task_trials_instruction.play(); });  // screen flip
      semantics_task_trials_instruction.status = PsychoJS.Status.STARTED;
    }
    if (t >= (semantics_task_trials_instruction.getDuration() + semantics_task_trials_instruction.tStart)     && semantics_task_trials_instruction.status === PsychoJS.Status.STARTED) {
      semantics_task_trials_instruction.stop();  // stop the sound (if longer than duration)
      semantics_task_trials_instruction.status = PsychoJS.Status.FINISHED;
    }
    
    // *semantics_task_trials_response* updates
    if (((semantics_task_trials_instruction.status == FINISHED)) && semantics_task_trials_response.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_response.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_response.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { semantics_task_trials_response.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { semantics_task_trials_response.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { semantics_task_trials_response.clearEvents(); });
    }
    
    if (semantics_task_trials_response.status === PsychoJS.Status.STARTED) {
      let theseKeys = semantics_task_trials_response.getKeys({keyList: ['1', '2'], waitRelease: false});
      _semantics_task_trials_response_allKeys = _semantics_task_trials_response_allKeys.concat(theseKeys);
      if (_semantics_task_trials_response_allKeys.length > 0) {
        semantics_task_trials_response.keys = _semantics_task_trials_response_allKeys[_semantics_task_trials_response_allKeys.length - 1].name;  // just the last key pressed
        semantics_task_trials_response.rt = _semantics_task_trials_response_allKeys[_semantics_task_trials_response_allKeys.length - 1].rt;
        semantics_task_trials_response.duration = _semantics_task_trials_response_allKeys[_semantics_task_trials_response_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    
    // *semantics_task_trials_order_instruction* updates
    if (t >= 4.5 && semantics_task_trials_order_instruction.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_order_instruction.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_order_instruction.frameNStart = frameN;  // exact frame index
      
      semantics_task_trials_order_instruction.setAutoDraw(true);
    }
    
    
    // *semantics_task_trials_proceed* updates
    if (t >= 4.5 && semantics_task_trials_proceed.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_trials_proceed.tStart = t;  // (not accounting for frame time here)
      semantics_task_trials_proceed.frameNStart = frameN;  // exact frame index
      
      semantics_task_trials_proceed.setAutoDraw(true);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of semantics_task_trialsComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function semantics_task_trialsRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'semantics_task_trials' ---
    for (const thisComponent of semantics_task_trialsComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('semantics_task_trials.stopped', globalClock.getTime());
    semantics_task_trials_word1.stop();  // ensure sound has stopped at end of Routine
    semantics_task_trials_word2.stop();  // ensure sound has stopped at end of Routine
    semantics_task_trials_instruction.stop();  // ensure sound has stopped at end of Routine
    semantics_task_trials_response.stop();
    // the Routine "semantics_task_trials" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var sem_trialMaxDurationReached;
var _key_resp_vmi_s_allKeys;
var sem_trialMaxDuration;
var sem_trialComponents;
function sem_trialRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'sem_trial' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    sem_trialClock.reset();
    routineTimer.reset();
    sem_trialMaxDurationReached = false;
    // update component parameters for each repeat
    semanticBlock_idx.setText(semanticBlock.thisN);
    field_s.setValue(field);
    field_s.secs=2;
    field_s.setVolume(1.0);
    word1_s.setValue(word1_sem);
    word1_s.secs=2;
    word1_s.setVolume(1.0);
    word2_s.setValue(word2_sem);
    word2_s.secs=2;
    word2_s.setVolume(1.0);
    instr_s.setValue(instruction_sem);
    instr_s.setVolume(1.0);
    key_resp_vmi_s.keys = undefined;
    key_resp_vmi_s.rt = undefined;
    _key_resp_vmi_s_allKeys = [];
    psychoJS.experiment.addData('sem_trial.started', globalClock.getTime());
    sem_trialMaxDuration = null
    // keep track of which components have finished
    sem_trialComponents = [];
    sem_trialComponents.push(semantics_task_title);
    sem_trialComponents.push(semanticBlock_idx);
    sem_trialComponents.push(fixation);
    sem_trialComponents.push(field_s);
    sem_trialComponents.push(word1_s);
    sem_trialComponents.push(word2_s);
    sem_trialComponents.push(instr_s);
    sem_trialComponents.push(key_resp_vmi_s);
    
    for (const thisComponent of sem_trialComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function sem_trialRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'sem_trial' ---
    // get current time
    t = sem_trialClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *semantics_task_title* updates
    if (t >= 0.0 && semantics_task_title.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semantics_task_title.tStart = t;  // (not accounting for frame time here)
      semantics_task_title.frameNStart = frameN;  // exact frame index
      
      semantics_task_title.setAutoDraw(true);
    }
    
    
    // *semanticBlock_idx* updates
    if (t >= 0.0 && semanticBlock_idx.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      semanticBlock_idx.tStart = t;  // (not accounting for frame time here)
      semanticBlock_idx.frameNStart = frameN;  // exact frame index
      
      semanticBlock_idx.setAutoDraw(true);
    }
    
    
    // *fixation* updates
    if (t >= 0.0 && fixation.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      fixation.tStart = t;  // (not accounting for frame time here)
      fixation.frameNStart = frameN;  // exact frame index
      
      fixation.setAutoDraw(true);
    }
    
    // start/stop field_s
    if (t >= 0.0 && field_s.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      field_s.tStart = t;  // (not accounting for frame time here)
      field_s.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ field_s.play(); });  // screen flip
      field_s.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 0.0 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (field_s.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= field_s.tStart + 0.5) {
        field_s.stop();  // stop the sound (if longer than duration)
        field_s.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop word1_s
    if (t >= 2 && word1_s.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      word1_s.tStart = t;  // (not accounting for frame time here)
      word1_s.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ word1_s.play(); });  // screen flip
      word1_s.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 2 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (word1_s.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= word1_s.tStart + 0.5) {
        word1_s.stop();  // stop the sound (if longer than duration)
        word1_s.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop word2_s
    if (t >= 4 && word2_s.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      word2_s.tStart = t;  // (not accounting for frame time here)
      word2_s.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ word2_s.play(); });  // screen flip
      word2_s.status = PsychoJS.Status.STARTED;
    }
    frameRemains = 4 + 2 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (word2_s.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      if (t >= word2_s.tStart + 0.5) {
        word2_s.stop();  // stop the sound (if longer than duration)
        word2_s.status = PsychoJS.Status.FINISHED;
      }
    }
    // start/stop instr_s
    if (t >= 6 && instr_s.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      instr_s.tStart = t;  // (not accounting for frame time here)
      instr_s.frameNStart = frameN;  // exact frame index
      
      psychoJS.window.callOnFlip(function(){ instr_s.play(); });  // screen flip
      instr_s.status = PsychoJS.Status.STARTED;
    }
    if (t >= (instr_s.getDuration() + instr_s.tStart)     && instr_s.status === PsychoJS.Status.STARTED) {
      instr_s.stop();  // stop the sound (if longer than duration)
      instr_s.status = PsychoJS.Status.FINISHED;
    }
    
    // *key_resp_vmi_s* updates
    if (((instr_s.status == FINISHED)) && key_resp_vmi_s.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      key_resp_vmi_s.tStart = t;  // (not accounting for frame time here)
      key_resp_vmi_s.frameNStart = frameN;  // exact frame index
      
      // keyboard checking is just starting
      psychoJS.window.callOnFlip(function() { key_resp_vmi_s.clock.reset(); });  // t=0 on next screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vmi_s.start(); }); // start on screen flip
      psychoJS.window.callOnFlip(function() { key_resp_vmi_s.clearEvents(); });
    }
    
    if (key_resp_vmi_s.status === PsychoJS.Status.STARTED) {
      let theseKeys = key_resp_vmi_s.getKeys({keyList: ['1', '2'], waitRelease: false});
      _key_resp_vmi_s_allKeys = _key_resp_vmi_s_allKeys.concat(theseKeys);
      if (_key_resp_vmi_s_allKeys.length > 0) {
        key_resp_vmi_s.keys = _key_resp_vmi_s_allKeys[_key_resp_vmi_s_allKeys.length - 1].name;  // just the last key pressed
        key_resp_vmi_s.rt = _key_resp_vmi_s_allKeys[_key_resp_vmi_s_allKeys.length - 1].rt;
        key_resp_vmi_s.duration = _key_resp_vmi_s_allKeys[_key_resp_vmi_s_allKeys.length - 1].duration;
        // a response ends the routine
        continueRoutine = false;
      }
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of sem_trialComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function sem_trialRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'sem_trial' ---
    for (const thisComponent of sem_trialComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('sem_trial.stopped', globalClock.getTime());
    field_s.stop();  // ensure sound has stopped at end of Routine
    word1_s.stop();  // ensure sound has stopped at end of Routine
    word2_s.stop();  // ensure sound has stopped at end of Routine
    instr_s.stop();  // ensure sound has stopped at end of Routine
    // update the trial handler
    if (currentLoop instanceof MultiStairHandler) {
      currentLoop.addResponse(key_resp_vmi_s.corr, level);
    }
    psychoJS.experiment.addData('key_resp_vmi_s.keys', key_resp_vmi_s.keys);
    if (typeof key_resp_vmi_s.keys !== 'undefined') {  // we had a response
        psychoJS.experiment.addData('key_resp_vmi_s.rt', key_resp_vmi_s.rt);
        psychoJS.experiment.addData('key_resp_vmi_s.duration', key_resp_vmi_s.duration);
        routineTimer.reset();
        }
    
    key_resp_vmi_s.stop();
    // the Routine "sem_trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset();
    
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


var end_sessionMaxDurationReached;
var end_sessionMaxDuration;
var end_sessionComponents;
function end_sessionRoutineBegin(snapshot) {
  return async function () {
    TrialHandler.fromSnapshot(snapshot); // ensure that .thisN vals are up to date
    
    //--- Prepare to start Routine 'end_session' ---
    t = 0;
    frameN = -1;
    continueRoutine = true; // until we're told otherwise
    end_sessionClock.reset(routineTimer.getTime());
    routineTimer.add(6.000000);
    end_sessionMaxDurationReached = false;
    // update component parameters for each repeat
    psychoJS.experiment.addData('end_session.started', globalClock.getTime());
    end_sessionMaxDuration = null
    // keep track of which components have finished
    end_sessionComponents = [];
    end_sessionComponents.push(end_session_text);
    
    for (const thisComponent of end_sessionComponents)
      if ('status' in thisComponent)
        thisComponent.status = PsychoJS.Status.NOT_STARTED;
    return Scheduler.Event.NEXT;
  }
}


function end_sessionRoutineEachFrame() {
  return async function () {
    //--- Loop for each frame of Routine 'end_session' ---
    // get current time
    t = end_sessionClock.getTime();
    frameN = frameN + 1;// number of completed frames (so 0 is the first frame)
    // update/draw components on each frame
    
    // *end_session_text* updates
    if (t >= 0 && end_session_text.status === PsychoJS.Status.NOT_STARTED) {
      // keep track of start time/frame for later
      end_session_text.tStart = t;  // (not accounting for frame time here)
      end_session_text.frameNStart = frameN;  // exact frame index
      
      end_session_text.setAutoDraw(true);
    }
    
    frameRemains = 0 + 6 - psychoJS.window.monitorFramePeriod * 0.75;// most of one frame period left
    if (end_session_text.status === PsychoJS.Status.STARTED && t >= frameRemains) {
      end_session_text.setAutoDraw(false);
    }
    
    // check if the Routine should terminate
    if (!continueRoutine) {  // a component has requested a forced-end of Routine
      return Scheduler.Event.NEXT;
    }
    
    continueRoutine = false;  // reverts to True if at least one component still running
    for (const thisComponent of end_sessionComponents)
      if ('status' in thisComponent && thisComponent.status !== PsychoJS.Status.FINISHED) {
        continueRoutine = true;
        break;
      }
    
    // refresh the screen if continuing
    if (continueRoutine && routineTimer.getTime() > 0) {
      return Scheduler.Event.FLIP_REPEAT;
    } else {
      return Scheduler.Event.NEXT;
    }
  };
}


function end_sessionRoutineEnd(snapshot) {
  return async function () {
    //--- Ending Routine 'end_session' ---
    for (const thisComponent of end_sessionComponents) {
      if (typeof thisComponent.setAutoDraw === 'function') {
        thisComponent.setAutoDraw(false);
      }
    }
    psychoJS.experiment.addData('end_session.stopped', globalClock.getTime());
    if (end_sessionMaxDurationReached) {
        end_sessionClock.add(end_sessionMaxDuration);
    } else {
        end_sessionClock.add(6.000000);
    }
    // Routines running outside a loop should always advance the datafile row
    if (currentLoop === psychoJS.experiment) {
      psychoJS.experiment.nextEntry(snapshot);
    }
    return Scheduler.Event.NEXT;
  }
}


function importConditions(currentLoop) {
  return async function () {
    psychoJS.importAttributes(currentLoop.getCurrentTrial());
    return Scheduler.Event.NEXT;
    };
}


async function quitPsychoJS(message, isCompleted) {
  // Check for and save orphaned data
  if (psychoJS.experiment.isEntryEmpty()) {
    psychoJS.experiment.nextEntry();
  }
  psychoJS.window.close();
  psychoJS.quit({message: message, isCompleted: isCompleted});
  
  return Scheduler.Event.QUIT;
}
