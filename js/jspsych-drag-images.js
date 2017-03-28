/**
 * jspsych-free-sort
 * plugin for drag-and-drop sorting of a collection of images
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 */


jsPsych.plugins['drag-images'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('drag-images', 'stimuli', 'image');

  plugin.trial = function(display_element, trial) {

    // default values
    trial.stim_height = trial.stim_height || '25%';
    trial.stim_width = trial.stim_width || '25%';
    trial.stim_bg = (typeof trial.stim_bg === 'undefined') ? '' : trial.stim_bg;
    trial.prompt = (typeof trial.prompt === 'undefined') ? '' : trial.prompt;
    trial.init_locations = (typeof trial.init_locations === 'undefined') ? '' : trial.init_locations;
    trial.prompt_location = trial.prompt_location || "above";
    trial.sort_area_width = trial.sort_area_width || 600;
    trial.sort_area_height = trial.sort_area_height || 600;
    trial.bg_width = trial.bg_width || '100%';
    trial.bg_height = trial.bg_height || '100%';
    trial.bg_locations = (typeof trial.bg_locations === 'undefined') ? '' : trial.bg_locations;
    trial.progress_report  = (typeof trial.progress_report === 'undefined') ? '' : trial.progress_report;
    trial.comment = (typeof trial.comment === 'undefined') ? '' : trial.comment;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    var start_time = (new Date()).getTime();

    display_element.append($("<div>",{
      "id":'left_panel',
      "css": {
        "float": "left"
      }
    }));

    display_element.append($("<div>",{
      "id":'right_panel',
      "css": {
        "float": "right"
      }
    }));
    // check if there is a prompt and if it is shown above
    if (trial.prompt && trial.prompt_location == "above") {
      $("#left_panel").append($('<div>', {
        "html": "<p>"+trial.prompt+"</p>",
        "id": "jspsych-drag-images-prompt",
        "class": "jspsych-free-sort-arena"
      }));
    }

    // display progress
    if (trial.progress_report == "true") {
      $("#right_panel").append($('<div>', {
        "html": "<p>"+"Number of questions <br>reviewed: "+(1+jsPsych.progress().current_trial_global)+"/"+jsPsych.progress().total_trials+"</p>",
        "id": "jspsych-progress-report",
        "class": "jspsych-free-sort-arena"
      }));
    }


    // display comment box
    if (trial.comment != "") {
      $("#left_panel").append($('<div>', {
        "html": "<p>Any comments?</p><textarea rows='5' style= 'margin:5px' placeholder = '"+trial.comment+"' id='comment'></textarea>",
        "id": "jspsych-drag-images-comment",
        "class": "jspsych-free-sort-arena",
        "css": {
          "margin": "10px 0 10px 0",
          "width": $("#jspsych-drag-images-prompt").css('width')
        }
      }));
    }

    // display button to continue
    $("#right_panel").append($('<button>', {
      "id": "jspsych-free-sort-done-btn",
      "class": "jspsych-btn jspsych-free-sort",
      "css": {
        "margin": "50px",
        "width": "120px"
      },
      "html": "Continue",
      "click": function() {
        var end_time = (new Date()).getTime();
        var rt = end_time - start_time;
        // gather data
        // get final position of all objects
        var final_locations = [];
        $('.jspsych-free-sort-draggable').each(function() {
          final_locations.push({
            "src": $(this).attr('src'),
            "x": $(this).css('left'),
            "y": $(this).css('top')
          });
        });

        var trial_data = {
          "init_locations": JSON.stringify(trial.init_locations),
          "moves": JSON.stringify(moves),
          "final_locations": JSON.stringify(final_locations),
          "rt": rt,
          "comment": document.getElementById('comment').value
        };

        // advance to next part
        display_element.html("");
        jsPsych.finishTrial(trial_data);
      }
    }));

    // display button to quit
    $("#right_panel").append($('<button>', {
      "id": "jspsych-free-sort-quit-btn",
      "class": "jspsych-btn jspsych-free-sort",
      "css": {
        "margin": "50px",
        "width": "120px"
      },
      "html": "Save and Quit",
      "click": function() {
        var save = function() {
          saveData(filename_i, jsPsych.data.dataAsJSON());
        }
        $.when( save() ).done(function(){
          display_element.html("<br><br><br>Thank you for your help in making our test. <br>Your effort is greatly appreciated! <br> Please close this window to return to our website.");
        });
      }
    }));

    display_element.append($('<div>', {
      "id": "jspsych-free-sort-arena",
      "class": "jspsych-free-sort-arena",
      "css": {
        "position": "relative",
        "width": trial.sort_area_width,
        "height": trial.sort_area_height
      }
    }));

    // check if prompt exists and if it is shown below
    if (trial.prompt && trial.prompt_location == "below") {
      display_element.append(trial.prompt);
    }

    // if init_locations is not defined, then randomly generate them
    if (trial.init_locations=="") {
      // store initial location data
      var init_locations = [];

      for (var i = 0; i < trial.stimuli.length; i++) {
        var coords = random_coordinate(trial.sort_area_width - trial.stim_width, trial.sort_area_height - trial.stim_height);

        init_locations.push({
          "src": trial.stimuli[i],
          "x": coords.x,
          "y": coords.y
        });
      }
      trial.init_locations = init_locations;
    }

    // display background
    if (trial.stim_bg!="") {
      $("#jspsych-free-sort-arena").append($('<img>', {
        "src": trial.stim_bg,
        "class": "jspsych-free-sort-bg",
        "css": {
          "position": "absolute",
          "top": trial.bg_locations.y,
          "left": trial.bg_locations.x,
          "width": trial.bg_width,
          "height": trial.bg_height
        }
      }));
    }

    // display images
    for (var i = 0; i < trial.stimuli.length; i++) {
      $("#jspsych-free-sort-arena").append($('<img>', {
        "src": trial.stimuli[i],
        "class": "jspsych-free-sort-draggable",
        "css": {
          "position": "absolute",
          "top": trial.init_locations[i].y,
          "left": trial.init_locations[i].x,
          "width": trial.stim_width,
          "height": trial.stim_height
        }
      }));
      trial.init_locations[i].src = trial.stimuli[i];
    }

    var moves = [];

    $('.jspsych-free-sort-draggable').draggable({
      containment: "#jspsych-free-sort-arena",
      scroll: false,
      stack: ".jspsych-free-sort-draggable",
      stop: function(event, ui) {
        moves.push({
          "src": event.target.src.split("/").slice(-1)[0],
          "x": ui.position.left,
          "y": ui.position.top
        });
      }
    });

  };

  // helper functions

  function random_coordinate(max_width, max_height) {
    var rnd_x = Math.floor(Math.random() * (max_width - 1));
    var rnd_y = Math.floor(Math.random() * (max_height - 1));

    return {
      x: rnd_x,
      y: rnd_y
    };
  }

  return plugin;
})();
