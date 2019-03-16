"use strict";

exports.primitives = {
  newWebSocket: function(path) {
    var loc = window.location;
    var proto = loc.protocol;
    proto = (proto === "https:") ? "wss:" : "ws:";
    var url;
    if (! path || path === "") {
      // append /ws/ to the path, but without the index.html (if present)
      url = proto + "//" + loc.host + loc.pathname;
      url = url.replace(/\/[^\/]*$/, "/ws/");
    } else {
      // use the path the caller wants
      if (! path.startsWith("/")) {
        path = "/" + path;
      }
      url = proto + "//" + loc.host + path;
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
