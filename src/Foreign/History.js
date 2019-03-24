// This module is a little wrapper over some needed bits of the HTML5 history api
// TODO: Use purescript-concur-react-router instead

// Set the current URL
// setUrl :: String -> Effect Unit
exports.setUrl = function(url) {
  // Since this an Effect, we need to return a function that will execute the effect.
  return function() {
    return window.history.pushState(null, null, url);
  };
};
