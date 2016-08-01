"use strict";

exports.spliceStr = function spliceStr(self) {
  return function(idx) {
    return function(rem) {
      return function(str) {
        return self.slice(0, idx) + str + self.slice(idx + Math.abs(rem));
      };
    };
  };
};

exports.spliceArr = function spliceArr(self) {
    return function(idx) {
        return function(rem) {
            return function(elems) {
                const ret = self.slice();
                Array.prototype.splice.apply(ret, [idx, rem].concat(elems));
                return ret;
            };
        };
    };
};
