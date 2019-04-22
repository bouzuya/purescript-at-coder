exports.getSolve = function (name) {
  // . = output/Test.Main/
  return require('../' + name).solve;
};
