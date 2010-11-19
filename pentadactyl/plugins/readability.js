/**
 * Copyright (c) 2009 by Eric Van Dewoestine
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 *
 * Plugin which integrates arc90's readability bookmarklet into vimperator.
 *
 * Usage:
 *   :readability
 *     use readability to format the current page.
 *
 * Configuration:
 *   In the command declaration below you can edit the readStyle, readSize, and
 *   readMargin variables to specify your preferred readability supported
 *   values.
 *
 * @version 0.1
 */
commands.add(["readability"],
  "Reformat the current page for readability using arc90's Readability bookmarklet.",
  function(args) {
    // TODO: would be nice if this would work without feeding it to
    // document.location
    window.content.document.location = "javascript:" +
      "(function(){" +
      "    readStyle='style-ebook';" +
      "    readSize='size-small';" +
      "    readMargin='margin-narrow';" +
      "    _readability_script=document.createElement('SCRIPT');" +
      "    _readability_script.type='text/javascript';" +
      "    _readability_script.src=" +
      "      'http://lab.arc90.com/experiments/readability/js/readability.js?x='+(Math.random());" +
      "    document.getElementsByTagName('head')[0].appendChild(_readability_script);" +
      "    _readability_css=document.createElement('LINK');" +
      "    _readability_css.rel='stylesheet';" +
      "    _readability_css.href=" +
      "      'http://lab.arc90.com/experiments/readability/css/readability.css';" +
      "    _readability_css.type='text/css';" +
      "    _readability_css.media='screen';" +
      "    document.getElementsByTagName('head')[0].appendChild(_readability_css);" +
      "    _readability_print_css=document.createElement('LINK');" +
      "    _readability_print_css.rel='stylesheet';" +
      "    _readability_print_css.href=" +
      "      'http://lab.arc90.com/experiments/readability/css/readability-print.css';" +
      "    _readability_print_css.media='print';" +
      "    _readability_print_css.type='text/css';" +
      "    document.getElementsByTagName('head')[0].appendChild(_readability_print_css);" +
      "    setTimeout(function(){" +
      "      var overlay = document.body;" +
      "      if (overlay) {" +
      "        overlay.className = '';" +
      "        overlay.background = '';" +
      "        overlay.bgColor = '';" +
      "        overlay.text = '';" +
      "        overlay.firstChild.className = '';" +
      "        overlay.style.backgroundColor = '#bbb';" +
      "        overlay.style.color = '#333';" +
      "      }" +
      "    }, 1000);" +
      "})();";
  }, {argCount: 0}
);
