"use strict";

exports.primitives = {
  newEventSource: function(url, auth) {
    var h = { headers:
              { 'Authorization': 'Bearer ' + auth}
            };
    return new window.EventSourcePolyfill(url, h);
  },
  onMessage: function(handler, es) {
    es.addEventListener("message", function (x) { handler(x)(); });
  },
  close: function(es) {
    es.close();
  },
  consoleHandler: function(x) {
    return function() {
      console.log("message:", x);
    };
  }
};
