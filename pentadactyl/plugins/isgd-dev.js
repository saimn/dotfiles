var INFO =
<plugin name="is.gd url shorter" version="0.2"
    href="http://"
    summary="is.gd url shorter">
  <author email="matias@russitto.com" href="http://russitto.com">Matias Russitto</author>
  <license href="http://opensource.org/licenses/mit-license.php">MIT</license>
  <project name="Pentadactyl" minVersion="1.0b2" maxVersion="1.0"/>
  <description>
    is.gd an url and copy it to clipboard
    <ul>
      <li>:is  - shorten buffer url and copy to clipboard</li>
      <li>:is [url]  - shorten [url] and copy to clipboard</li>
      <li>;g  - shorten hint and copy to clipboard</li>
    </ul>
  </description>
</plugin>;

function IsGdShortener (args) {

  var url, post_data, req_url, json_resp, httpGet;

  httpGet = function (url, data, callback) {
    try {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.mozBackgroundRequest = true;

      if (callback) {
        xmlhttp.onreadystatechange = function () {
          if (xmlhttp.readyState == 4) {
            callback(xmlhttp);
          }
        };
      }

      url += '?' + data;
      xmlhttp.open("GET", url, !!callback);
      xmlhttp.send(data);
      return xmlhttp;
    }
    catch (e) {
      dactyl.echo("Error opening " + url + ": " + e, 1);
    }
  };

  // for some reason util.copyToClipboard is undefined so hack away
  var copyToClipboard = function (str, verbose) {
    const clipboardHelper = Components.classes["@mozilla.org/widget/clipboardhelper;1"].getService(Ci.nsIClipboardHelper);
    clipboardHelper.copyString(str);

    if (verbose) {
      dactyl.echo("Yanked " + str, commandline.FORCE_SINGLELINE);
    }
  };

  post_data = 'format=simple&url=';
  // req_url = "http://v.gd/create.php";
  req_url = "http://is.gd/create.php";
  if (args[0] && args[0] != '') {
    url = args[0];
  } else {
    url = buffer.URL;
  }

  post_data = post_data + encodeURIComponent(url);

  httpGet(req_url, post_data, function (req) {

    // getting back a 201 status in testing
    if (req.status == 200 || req.status==201) {
      var url = req.responseText;

      copyToClipboard('harding', true);
      copyToClipboard(url, true);
    } else {
      dactyl.echo("Error contacting is.gd!\n");
    }
  });
}

group.commands.add(["is", "isgd"], "is.gd an url and copy it to clipboard", IsGdShortener);

hints.addMode('g', "Generate curl command for a form", function(elem) {
  var url = elem.getAttribute("href");

  if (!url || /^javascript:/.test(url))
    return;
  IsGdShortener([url]);
});

/* vim:se sts=2 sw=2 et: */
