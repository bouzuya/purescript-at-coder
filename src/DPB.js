exports.modifyImpl = function (i) {
  return function (f) {
    return function (xs) {
      return function () {
        xs[i] = f(xs[i]);
        return {};
      };
    };
  };
};
