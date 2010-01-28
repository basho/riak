var Riak = function() {

  return {
    getClassName: function(obj) {
      if (obj && obj.constructor && obj.constructor.toString) {
        var arr = obj.constructor.toString().match(/function\s*(\w+)/);
        if (arr && arr.length == 2) {
	  return arr[1];
        }
      }
      return undefined;
    },
    mapValues: function(value, key_data, arg) {
      var data = value["values"][0]["data"];
      if (Riak.getClassName(data) !== "Array") {
	return [data];
      }
      else {
	return data;
      }},
     mapValuesJson: function(value, key_data, arg) {
      return [JSON.parse(value)];
    },
    reduceSum: function(values, arg) {
      return [values.reduce(function(prev, curr, index, array) { return prev + curr; })];
    },
    reduceMin: function(values, arg) {
      values.sort();
      return [values[0]];
    },
    reduceMax: function(values, arg) {
      values.sort().reverse();
      return [values[0]];
    },
    reduceSort: function(value, arg) {
      var c = null;
      if (arg) {
	c = eval(arg);
      }
      if(c) {
	return value.sort(c);
      }
      else {
	return value.sort();
      }
    },
    reduceLimit: function(value, arg) {
      return value.slice(0, arg - 1);
    },
    reduceSlice: function(value, arg) {
      var start = arg[0];
      var end = arg[1];
      if (end > value.length) {
	return value;
      }
      else {
	return value.slice(start, end);
      }
    }
  }
}();
