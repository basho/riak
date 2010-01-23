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
    reduceAverage: function(values, arg) {
      var total = Riak.reduceSum(values, arg);
      return [total / values.length];
    }
  }
}();
