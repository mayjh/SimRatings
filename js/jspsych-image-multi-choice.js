/**
 * jspsych-survey-multi-choice
 * a jspsych plugin for multiple choice survey questions
 *
 * Shane Martin
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins['image-multi-choice'] = (function() {

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    var plugin_id_name = "jspsych-image-multi-choice";
    var plugin_id_selector = '#' + plugin_id_name;
    var _join = function( /*args*/ ) {
      var arr = Array.prototype.slice.call(arguments, _join.length);
      return arr.join(separator = '-');
    }

    // trial defaults
    trial.images = typeof trial.images == 'undefined' ? null : trial.images;
    trial.preamble = typeof trial.preamble == 'undefined' ? "" : trial.preamble;
    trial.required = typeof trial.required == 'undefined' ? null : trial.required;
    trial.horizontal = typeof trial.required == 'undefined' ? false : trial.horizontal;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

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

    // show the images
    display_element.append($('<table>', {
      "id": 'jspsych-image-multi-choice-table',
      "css": {"text-align": "center","vertical-align": "bottom","margin":"auto","float":"left"}
    }));
    //append four images as two rows in the table
    // add question text
    $("#jspsych-image-multi-choice-table").append($('<tr>',{"id": 'jspsych-image-multi-choice-ims0'}));
    $("#jspsych-image-multi-choice-ims0").append($('<td><p class="' + plugin_id_name + '-text survey-multi-choice">'+trial.questions[0] +'</p></td>'));
    $("#jspsych-image-multi-choice-table").append($('<tr>',{"id": 'jspsych-image-multi-choice-ims1'}));
    $("#jspsych-image-multi-choice-ims1").append($('<td><img src='+trial.images[0] +' class="jspsych-sim-image" > </td>'));//style="width:350px;height:350px;"
    $("#jspsych-image-multi-choice-ims1").append($('<td><img src='+trial.images[1] +' class="jspsych-sim-image" > </td>'));
    $("#jspsych-image-multi-choice-table").append($('<tr>',{"id": 'jspsych-image-multi-choice-ims2'}));
    $("#jspsych-image-multi-choice-ims2").append($('<td><img src='+trial.images[2] +' class="jspsych-sim-image" > </td>'));//style="width:350px;height:350px;"
    $("#jspsych-image-multi-choice-ims2").append($('<td><img src='+trial.images[3] +' class="jspsych-sim-image" > </td>'));
    $('.jspsych-sim-image').css({
      "width":"200px",
      "height":"200px",
      "border": "solid 2px"
    });

    // form element
    var trial_form_id = _join(plugin_id_name, "form");
    display_element.append($('<form>', {
      "id": trial_form_id
    }));
    var $trial_form = $("#" + trial_form_id);

    // show preamble text
    var preamble_id_name = _join(plugin_id_name, 'preamble');
    $trial_form.append($('<div>', {
      "id": preamble_id_name,
      "class": preamble_id_name
    }));
    $('#' + preamble_id_name).html(trial.preamble);

    // add multiple-choice questions
    for (var i = 0; i < trial.questions.length; i++) {

      // create question container
      var question_classes = [_join(plugin_id_name, 'question')];
      if (trial.horizontal) {
        question_classes.push(_join(plugin_id_name, 'horizontal'));
      }

      $trial_form.append($('<div>', {
        "id": _join(plugin_id_name, i),
        "class": question_classes.join(' '),
        "css": {"float":"right"}
      }));

      var question_selector = _join(plugin_id_selector, i);

      // create option radio buttons
      for (var j = 0; j < trial.options[i].length; j++) {
        var option_id_name = _join(plugin_id_name, "option", i, j),
          option_id_selector = '#' + option_id_name;

        // add radio button container
        $(question_selector).append($('<div>', {
          "id": option_id_name,
          "class": _join(plugin_id_name, 'option')
        }));

        // add label and question text
        var option_label = '<label class="' + plugin_id_name + '-text">' + trial.options[i][j] + '</label>';
        $(option_id_selector).append(option_label);

        // create radio button
        var input_id_name = _join(plugin_id_name, 'response', i);
        $(option_id_selector + " label").prepend('<input type="radio" name="' + input_id_name + '" value="' + trial.options[i][j] + '">');
      }

      if (trial.required && trial.required[i]) {
        // add "question required" asterisk
        $(question_selector + " p").append("<span class='required'>*</span>")

        // add required property
        $(question_selector + " input:radio").prop("required", true);
      }
    }

    // add submit button
    $trial_form.append($('<div>', {
      // 'type': 'submit',
      // 'id': plugin_id_name + '-next',
      // 'class': plugin_id_name + ' jspsych-btn',
      // 'value': 'Submit Answers'
      'html': '<input type="submit" id="'+plugin_id_name+'-next" class="'+plugin_id_name+' jspsych-btn" value="Submit Answers">',
      'css': {'margin':'auto',"text-align":"center","padding":"20px"}
    }));


    $trial_form.submit(function(event) {

      event.preventDefault();

      // measure response time
      var endTime = (new Date()).getTime();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};
      $("div." + plugin_id_name + "-question").each(function(index) {
        var id = "Q" + index;
        var val = $(this).find("input:radio:checked").val();
        var obje = {};
        obje[id] = val;
        $.extend(question_data, obje);
      });

      // save data
      var trial_data = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.html('');

      // next trial
      jsPsych.finishTrial(trial_data);
    });

    var startTime = (new Date()).getTime();
  };

  return plugin;
})();
