/*
 * Simple extension to maximize the width of the notebook.
 * To install it, use:
 *
 *   jupyter nbextension install /path/to/fullwidth.js -user --symlink
 *   jupyter nbextension enable fullwidth --user
 *
 */

define([
  'base/js/namespace',
  'jquery',
], function(Jupyter, $) {
  "use strict";

  var load_extension = function() {
    console.info('>>>> Loading fullwidth extension !');
    var fullwidth = false;

    Jupyter.toolbar.add_buttons_group([
      {
        'label'   : 'Full width',
        'icon'    : 'fa-arrows-alt',
        'callback': function () {
          if (fullwidth) {
            $('#notebook-container').css("width", "");
          } else {
            $('#notebook-container').css("width", "98%");
          }
          fullwidth = !fullwidth;
        }
      }
    ]);
  };

  var extension = {
    load_jupyter_extension : load_extension,
    load_ipython_extension : load_extension
  };
  return extension;
});
