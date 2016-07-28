"use strict";

function splice(self) {
  return function(idx) {
    return function(rem) {
      return function(str) {
        return self.slice(0, idx) + str + self.slice(idx + Math.abs(rem));
      }
    }
  }
}

exports.spliceStr = splice;
exports.spliceArr = splice;
