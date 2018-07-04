"use strict";

exports.primitives = {
  setScrollTop: function (elem, x) {
    elem.scrollTop = x;
  },
  scrollIntoViewTop: function (elem, b) {
    elem.scrollIntoView(b);
  },
  scrollIntoView: function (elem) {
    elem.scrollIntoView();
  },
};
