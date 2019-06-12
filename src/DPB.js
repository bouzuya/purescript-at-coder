exports.modify = function (i, f, xs) {
  return function () {
    xs[i] = f(xs[i]);
    return {};
  };
};

exports.peek = function (i, xs) {
  return function () {
    return xs[i];
  };
};

exports.poke = function (i, a, xs) {
  return function () {
    xs[i] = a;
    return {};
  };
};
