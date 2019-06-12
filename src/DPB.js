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

exports.solve1 = function (args) {
  var h, h2;
  var n = args.n;
  var k = args.k;
  var hs = args.hs;
  var costs = new Array(n).fill(Math.pow(10, 9) * 2);
  costs[0] = 0;
  for (var i = 1; i < n; i++) {
    h = hs[i];
    for (var j = Math.max(0, (i - k)); j < i; j++) {
      h2 = hs[j];
      costs[i] = Math.min(costs[i], costs[j] + (Math.abs(h2 - h) | 0));
    }
  }
  return costs[n - 1] | 0;
};
