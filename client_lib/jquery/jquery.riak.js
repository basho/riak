(function($) {
  var riakSuccessHandler = function(data, status) { };
  $.riakGetResource = function(bucket, key, options) {
    var settings = $.extend({ifModified: true,
	  cache: true,
	  success: riakSuccessHandler}, options || {});
    settings["url"] = "/raw/" + bucket + "/" + key;
    settings["type"] = "GET";
    $.ajax(settings);
  };

  $.riakPutResource = function(bucket, key, value, options) {
    var settings = $.extend({contentType: "application/json",
	  success: riakSuccessHandler}, options || {});
    settings["url"] = "/raw/" + bucket + "/" + key;
    settings["data"] = value;
    settings["type"] = "PUT";
    $.ajax(settings);
  };

  $.riakDeleteResource = function(bucket, key, options) {
    var settings = $.extend({success: riakSuccessHandler}, options || {});
    settings["url"] = "/raw/" + bucket + "/" + key;
    settings["type"] = "DELETE";
    $.ajax(settings);

  };

  $.riakMapReduce = function(targets, phases, options) {
    query = {inputs: targets,
	     query: buildQuery(phases)};
    var settings = $.extend({success: riakSuccessHandler}, options || {});
    settings["url"] = "/mapred";
    settings["type"] = "POST";
    settings["contentType"] = "application/json";
    settings["dataType"] = "json";
    settings["data"] = $.toJSON(query);
    $.ajax(settings);
  };

  function buildQuery(phases) {
    return phases.map(function(phase) { return buildPhase(phase); }, phases);
  }

  function buildPhase(phase) {
    var handler = null;
    var keepResults = phase["keep"] === true;

    if (phase["map"] !== undefined) {
      return {map: {language: "javascript", source: phase["map"].toString(), keep: keepResults}};
    }
    else if (phase["reduce"] !== undefined) {
      return {reduce: {language: "javascript", source: phase["reduce"].toString(), keep: keepResults}};
    }
    else {
      throw("Illegal phase definition");
    }
  }
 })(jQuery);
