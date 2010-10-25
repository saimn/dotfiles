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
        //     "wikipedia:minimal",
        //     "en.wikipedia.org",
        //     "wikipedia.css",
        // ],

        // [
        //     "wikipedia:minimal",
        //     "fr.wikipedia.org",
        //     "wikipedia.css",
        // ],

        // [
        //     "archlinux-forums:minimal",
        //     "bbs.archlinux.org",
        //     "archlinux.forum.css",
        // ],

        [
            "camptocamp:dark",
            "camptocamp.org",
            "camptocamp-dark.css",
        ],

        [
            "youtube:dark-minimal",
            "youtube.com",
            "youtube.css",
        ],

    ];

    // Get style directory
    var styledir = io.getRuntimeDirectories("styles")[0].path;

    for ([i, [name, filter, file]] in Iterator(userstyles))
    {
      // Remove all sheets with this filter
      styles.removeSheet(false, name)

      // Add the sheet
      styles.addSheet(false, name, filter,
        File(styledir+"/"+file).read())
    }
}

// Initial load
loaduserstyles()

// Add it as a command
commands.add(
    ["loaduserstyles"],
    "Load all user styles",
    loaduserstyles
);
