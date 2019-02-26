"use strict";

exports.primDecodePart = function(part) {
  var str = atob(part.replace(/_/g, '/').replace(/-/g,'+'));
  return JSON.parse(str);
};
