"use strict";

exports.displayTimestamp = function(s) {
  var d = new Date(s);
  return d.toLocaleString();
}
