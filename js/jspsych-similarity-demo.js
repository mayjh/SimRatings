/**
 * jspsych-similarity.js
 * Josh de Leeuw
 *
 * This plugin create a trial where two images are shown sequentially, and the subject rates their similarity using a slider controlled with the mouse.
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins.similarity_demo = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('similarity_demo', 'stimuli', 'image');

  plugin.trial = function(display_element, trial) {

    // default parameters
    trial.labels = (typeof trial.labels === 'undefined') ? ["Most dissimilar", "Somewhat dissimilar", "Somewhat similar", "Most similar"] : trial.labels;
    trial.intervals = trial.intervals || 7;
    trial.show_ticks = (typeof trial.show_ticks === 'undefined') ? true : trial.show_ticks;

    trial.show_response = trial.show_response || "POST_STIMULUS";

    trial.timing_first_stim = trial.timing_first_stim || 3000; // default 1000ms
    trial.timing_second_stim = trial.timing_second_stim || 3000; // -1 = inf time; positive numbers = msec to display second image.
    trial.timing_image_gap = trial.timing_image_gap || 10000; // default 1000ms

    trial.is_html = (typeof trial.is_html === 'undefined') ? false : trial.is_html;
    trial.prompt = (typeof trial.prompt === 'undefined') ? '' : trial.prompt;
    trial.progress_report  = (typeof trial.progress_report === 'undefined') ? '' : trial.progress_report;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    // this array holds handlers from setTimeout calls
    // that need to be cleared if the trial ends early
    var setTimeoutHandlers = [];

    // add border
    display_element.css({
      'width': '800',
      'height': '660',
      'margin': 'auto',
      'margin-top': '15px',
      'position': 'relative',
      'padding': '20px',
      'border': '2px solid',
      'border-radius': '25px'
    });

    // create right panel to hold the progress_report
    display_element.append($("<div>",{
      "id":'right_panel',
      "css": {
        "float": "right"
      }
    }));

    // display progress
    if (trial.progress_report == "true") {
      $("#right_panel").append($('<div>', {
        "html": "<p>"+"Progress: "+(jsPsych.progress().current_trial_global)+"/"+(jsPsych.progress().total_trials-1)+"</p>",
        "id": "jspsych-progress-report",
        "class": "jspsych-free-sort-arena",
        "css": {"border-style": "none"}
      }));
    }

    // if prompt is set, show prompt
    if (trial.prompt !== "") {
      display_element.append(trial.prompt);
    }

    // show the images
    display_element.append($('<table>', {
      "id": 'jspsych-sim-table',
      "css": {"text-align": "center","vertical-align": "bottom","margin":"auto"}
    }));
    //append two images as a row in the table
    $("#jspsych-sim-table").append($('<tr>',{"id": 'jspsych-sim-ims'}));
    $("#jspsych-sim-ims").append($('<td><img src='+trial.stimuli[0] +' class="jspsych-sim-image" > </td>'));//style="width:350px;height:350px;"
    $("#jspsych-sim-ims").append($('<td><img src='+trial.stimuli[1] +' class="jspsych-sim-image" > </td>'));
    $('.jspsych-sim-image').css({
      "width":"350px",
      "height":"350px",
      "border": "solid 2px"
    });

    $("#jspsych-sim-table").append($('<tr>',{"id": 'jspsych-sim-slider'}));
    setTimeoutHandlers.push(setTimeout(function() {
      showBlankScreen();
    }, trial.timing_first_stim));


    function showBlankScreen() {
      $('.jspsych-sim-image').css('visibility', 'hidden');
    }

    show_response_slider(display_element, trial);

    function show_response_slider(display_element, trial) {

      var startTime = (new Date()).getTime();

      // create slider
      display_element.append($('<div>', {
        "id": 'slider',
        "class": 'sim',
        "css": {"width": "50%","margin":"auto"}
      }));

      $("#slider").slider({
        value: Math.ceil(trial.intervals / 2),
        min: 1,
        max: trial.intervals,
        step: 1,
      });

      // show tick marks
      if (trial.show_ticks) {
        for (var j = 1; j < trial.intervals - 1; j++) {
          $('#slider').append('<div class="slidertickmark"></div>');
        }

        $('#slider .slidertickmark').each(function(index) {
          var left = (index + 1) * (100 / (trial.intervals - 1));
          $(this).css({
            'position': 'absolute',
            'left': left + '%',
            'width': '1px',
            'height': '100%',
            'background-color': '#222222'
          });
        });
      }

      // create labels for slider
      display_element.append($('<ul>', {
        "id": "sliderlabels",
        "class": 'sliderlabels',
        "css": {
          "width": "50%",
          "height": "3em",
          "margin": "auto",//"10px 0px 0px 0px",
          "padding": "0px",
          "display": "block",
          "position": "relative"
        }
      }));

      for (var j = 0; j < trial.labels.length; j++) {
        $("#sliderlabels").append('<li>' + trial.labels[j] + '</li>');
      }

      // position labels to match slider intervals
      var slider_width = $("#slider").width();
      var num_items = trial.labels.length;
      var item_width = slider_width / num_items;
      var spacing_interval = slider_width / (num_items - 1);

      $("#sliderlabels li").each(function(index) {
        $(this).css({
          'display': 'inline-block',
          'width': item_width + 'px',
          'margin': 'auto',
          'padding': '0px',
          'text-align': 'center',
          'position': 'absolute',
          'left': (spacing_interval * index) - (item_width / 2)
        });
      });

      //  create button
      display_element.append($('<div>', {
        'id': 'next_div',
        'class': 'sim',
        'html': '<button class= "jspsych-btn" type="button" id="next">Submit Answer</button>',
        'css': {"margin": "auto","text-align":"center","width":"50%","padding":"10px"}
      }));

      $("#next").click(function() {
        var endTime = (new Date()).getTime();
        var response_time = endTime - startTime;

        // kill any remaining setTimeout handlers
        for (var i = 0; i < setTimeoutHandlers.length; i++) {
          clearTimeout(setTimeoutHandlers[i]);
        }

        var score = $("#slider").slider("value");
        var trial_data = {
          "sim_score": score,
          "rt": response_time,
          "stimulus": JSON.stringify([trial.stimuli[0], trial.stimuli[1]])
        };
        // goto next trial in block
        display_element.html('');
        jsPsych.finishTrial(trial_data);
      });
    }
  };
  return plugin;
})();
