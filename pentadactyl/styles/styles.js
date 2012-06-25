/**
 * Adds all the userstyles to vimperator
 *
 */

var loaduserstyles = function()
{
    /// Userstyles
    var userstyles = [
        [
            "global",
            "*",
            "global.css",
        ],

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

    for ([i, [name, filter, file]] in Iterator(userstyles))
    {
      // Remove all sheets with this filter
      styles.user.remove(name)

      // Add the sheet
      styles.user.add(name, filter,
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
