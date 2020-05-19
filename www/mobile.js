
/* Determine whether app is being accessed from a modbile device
 from: https://stackoverflow.com/questions/46820481/r-shiny-app-detect-mobile 
 note: "Here's a way to detect a (small) mobile device based on the screen size. Note this deliberately excludes tablets with a larger screen. The following snippet checks if the screen size is below 768px and sends the result to the Shiny server using onInputChange as an input named is_mobile_device. The check is only done once when the page loads and the Shiny UI has finished loading."
*//*
$(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia("only screen and (max-width: 768px)").matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
*/

/* from: https://g3rv4.com/2017/08/shiny-detect-mobile-browsers */
var isMobileBinding = new Shiny.InputBinding();
$.extend(isMobileBinding, {
  find: function(scope) {
    return $(scope).find(".mobile-element");
    callback();
  },
  getValue: function(el) {
    return /((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent)
  },
  setValue: function(el, value) {
  },
  subscribe: function(el, callback) {
  },
  unsubscribe: function(el) {
  }
});

Shiny.inputBindings.register(isMobileBinding);