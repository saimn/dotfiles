/**
 * Adds all the userstyles to vimperator
 *
 */

var loaduserstyles = function()
{
    /// Userstyles
    var userstyles = [
        // [
        //     "global",
        //     "*",
        //     "global.css",
        // ],

        // [
        //     "archlinux-forums:minimal",
        //     "bbs.archlinux.org",
        //     "archlinux.forum.css",
        // ],

        // [
        //     "camptocamp:clear",
        //     "www.camptocamp.org",
        //     "camptocamp-clear.css",
        // ],

        [
            "camptocamp:dark",
            "www.camptocamp.org",
            "camptocamp-dark.css",
        ],

    ];

    // Get style directory
    var styledir = io.getRuntimeDirectories("styles")[0].path;

    for(var i = 0; i < userstyles.length; ++i)
    {
      // Remove all sheets with this filter
      var name = userstyles[i][0];
      var filter = userstyles[i][1];
      var file = userstyles[i][2];

      styles.removeSheet(false, name)

      // Add the sheet
      styles.addSheet(false, name, filter,
        File(styledir+"/"+file).read())
    }
}

// Initial load
loaduserstyles()

// Add it as a command
group.commands.add(
    ["loaduserstyles"],
    "Load all user styles",
    loaduserstyles
);
