"use strict";
XML.ignoreWhitespace = false;
XML.prettyPrinting = false;
var INFO =
<plugin name="tinyurl"
        version="0.1"
        href="http://dactyl.sf.net/pentadactyl/plugins#browser-improvements-plugin"
        summary="TinyURL for Pentadactyl"
        xmlns={NS}>
    <author mail="konbu.komuro@gmail.com" homepage="http://d.hatena.ne.jp/hogelog/">hogelog</author>
    <author mail="saikat@cs.cornell.edu" homepage="http://saikat.guha.cc/">saikat</author>
    <license href="http://opensource.org/licenses/mit-license.php">MIT</license>
    <project name="Pentadactyl" min-version="1.0"/>
    <p>
        This plugin helps construct and expand TinyURL links.
    </p>
    <item>
        <tags>:tinyurl</tags>
        <spec>:tinyurl <oa>url</oa></spec>
        <description>
            <p>
                Create a TinyURL for <oa>url</oa>. If no <oa>url</oa> is given,
                the current URL is used. The created TinyURL is echoed and copied
                to the clipboard.
            </p>

            <example>:tinyurl http://www.google.com</example>
        </description>
    </item>
    <item>
        <tags>:expandurl</tags>
        <spec>:expandurl <a>tinyurl</a></spec>
        <description>
            <p>
                Expand the URL for <a>tinyurl</a>. The expanded URL is echoed and
                copied to the clipboard.
            </p>
        </description>
    </item>
</plugin>;

const TinyAPI = 'http://tinyurl.com/api-create.php?url=';

group.commands.add(['tinyurl'], 'echo and copy TinyURL',
    function(args) {
        dactyl.clipboardWrite(tiny.getTiny(args.length==0 ? buffer.URL : args.string), true)
    }, {argCount:'?'});

group.commands.add(['expandurl'], 'expand TinyURL',
    function(args) {
        dactyl.clipboardWrite(tiny.getExpand(args.string), true)
    }, {argCount:'1'});

var tiny = plugins.tinyurl = {
    getTiny: function (url) {
        return util.httpGet(TinyAPI+encodeURIComponent(url)).responseText
    },

    getExpand: function (url) {
        return util.httpGet(url).channel.name
    }
};
// vim: fdm=marker sw=4 ts=4 et:
