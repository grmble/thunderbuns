"use strict";

exports.primitives = {
  newWebSocket: function(url) {
    if (! url || url === "") {
      var loc = window.location;
      var proto = loc.protocol;
      proto = (proto === "https") ? "wss" : "ws";
      url = proto + "://" + loc.host + "/";
    }
    console.log("creating new websocket for " + url);
    return new window.WebSocket(url);
  },
  onOpen: function(handler, ws) {
    ws.addEventListener("open", function(x) { handler(x)(); });
  },
  onMessage: function(handler, ws) {
    ws.addEventListener("message", function (x) { handler(x)(); });
  },
  onClose: function(handler, ws) {
    ws.addEventListener("close", function (x) { handler(x)(); });
  },
  onError: function(handler, ws) {
    ws.addEventListener("error", function (x) { handler(x)(); });
  },
  close: function(ws) {
    ws.close();
  },
  send: function(data, ws) {
    ws.send(data);
  },
  consoleHandler: function(x) {
    return function() {
      console.log("message:", x);
    };
  }
};
