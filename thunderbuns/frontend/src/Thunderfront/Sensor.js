"use strict";

function differentialSensor(pred) {
  var _sensor = { pred: pred,
                  primed: false,
                  last: true
                };

  _sensor.filter = function(ev) {
    return function() {
      if (! _sensor.primed) {
        // console.log("not primed", ev);
        return false;
      }

      var reading = _sensor.pred(ev)();
      var last = _sensor.last;
      _sensor.last = reading;

      if (last) {
        // console.log("not false-true boundary", ev);
        return false;
      }

      if (! reading) {
        // console.log("predicate false", ev);
        return false;
      }

      // console.log("sensor is firing, will have to be primed again", ev);
      _sensor.primed = false;
      return true;
    };
  };

  return _sensor;
}

function sensor(pred) {
  var _sensor = { pred: pred,
                  primed: false
                };

  _sensor.filter = function(ev) {
    return function() {
      if (! _sensor.primed) {
        // console.log("not primed", ev);
        return false;
      }

      var reading = _sensor.pred(ev)();

      if (! reading) {
        // console.log("predicate false", ev);
        return false;
      }

      // console.log("sensor is firing, will have to be primed again", ev);
      _sensor.primed = false;
      return true;
    };
  };

  return _sensor;
}

function prime(_sensor) {
  return function() {
    _sensor.primed = true;
  };
}

exports.primitives = {
  sensor: sensor,
  differentialSensor: differentialSensor,
  prime: prime,
};
