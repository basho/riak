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

 })(jQuery);
