var Riak = function() {

  return {
    mapIdentity: function(values, key_data, arg) { return values; },
    mapSelectField: function(values, key_data, fieldName) { if (values instanceof Array) {
	if (values.length == 1 && (values[0] instanceof Array && values[0].length > 0)) {
	  values = values[0];
        }
	return values.map(function(value) { value[fieldName]; });
      }
    },
    reduceUnion: function(values, arg) {
      var retval = [];
      values.foreach(function(value) {
	  if (retval.indexOf(value) == -1) {
	    retval[retval.length] = value;
	  } });
      return [retval];
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
    reduceAverage: function(values, arg) {
      var total = Riak.reduceSum(values, arg);
      return [total / values.length];
    },
    reduceRandom: function(values, arg) {
      var index = Math.floor(Math.random() * values.length - 1);
      return [values[index]];
    }
  }
}();
