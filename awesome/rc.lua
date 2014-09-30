-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- User libraries
local functions = require("functions")
-- local revelation = require("revelation")
local vicious = require("vicious")
-- require("vicious.contrib")
local scratch = require("scratch")
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- This is used later as the default terminal and editor to run.
local altkey = "Mod1"
local modkey = "Mod4"

local home       = os.getenv("HOME")
local host       = oscapture("hostname")
local config     = awful.util.getdir("config")
local exec       = awful.util.spawn
local sexec      = awful.util.spawn_with_shell
local scount     = screen.count()
local terminal   = "urxvtc"
local browser    = os.getenv("BROWSER") or "firefox"
local mail_cmd   = terminal.." -T Mutt -name Mutt -e mutt"
local editor     = os.getenv("EDITOR") or "vim"
local editor_cmd = terminal .. " -e " .. editor
local filemgr    = "pcmanfm"
local htop_cmd   = terminal.." -name htop -geometry 80x7 -e htop"
local lock_cmd   = "xscreensaver-command -lock"

-- Themes define colours, icons, and wallpapers
-- beautiful.init("/usr/share/awesome/themes/default/theme.lua")
beautiful.init(home .. "/.config/awesome/themes/niceandclean/theme.lua")

-- Note to include a separate config file:
-- dofile(config .. "/keybindings.lua")

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}


-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}


-- {{{ Tags
tags = {
  names  = { " mail ", " web ", " term ", " code ", " misc ", 6, 7, 8, " media " },
  layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
             layouts[1], layouts[1], layouts[1], layouts[1]
}}

for s = 1, screen.count() do
    tags[s] = awful.tag(tags.names, s, tags.layout)
    -- awful.tag.setproperty(tags[s][5], "mwfact", 0.13)
    -- awful.tag.setproperty(tags[s][6], "hide",   true)
    -- awful.tag.setproperty(tags[s][7], "hide",   true)
    -- awful.tag.setproperty(tags[s][8], "hide",   true)
end
-- }}}


-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = {
    { "awesome", myawesomemenu, beautiful.awesome_icon },
    { "Suspend", "sudo pm-suspend" },
    { "Reboot", "systemctl reboot" },
    { "Shutdown", "systemctl poweroff" },
    { "terminal", terminal },
    { "htop", htop_cmd },
    { "browser", browser },
    { "mail", mail_cmd },
    { "files", filemgr },
    { "lock", lock_cmd },
    { "quit", awesome.quit }
  }
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}


-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

-- Keyboard map indicator and changer
kbdcfg = {}
kbdcfg.cmd = "setxkbmap"
kbdcfg.layout = { { "fr", "oss" }, { "us", "altgr-intl" } }
kbdcfg.current = 1  -- fr is our default layout
kbdcfg.widget = wibox.widget.textbox()
kbdcfg.widget:set_text(" " .. kbdcfg.layout[kbdcfg.current][1] .. " ")
kbdcfg.switch = function ()
  kbdcfg.current = kbdcfg.current % #(kbdcfg.layout) + 1
  local t = kbdcfg.layout[kbdcfg.current]
  kbdcfg.widget:set_text(" " .. t[1] .. " ")
  os.execute( kbdcfg.cmd .. " " .. t[1] .. " " .. t[2] )
end

 -- Mouse bindings
kbdcfg.widget:buttons(
 awful.util.table.join(awful.button({ }, 1, function () kbdcfg.switch() end))
)

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top",
                               height = 20,
                               screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(kbdcfg.widget)
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- Client bindings
clientbuttons = awful.util.table.join(
    awful.button({ },        1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}


-- {{{ Key bindings
--
-- {{{ Global keys
globalkeys = awful.util.table.join(
    -- {{{ Applications
    awful.key({ modkey, "Shift" }, "e", function () exec("emacsclient -n -c") end),
    awful.key({ modkey, "Shift" }, "f", function () exec(browser) end),
    awful.key({ modkey, "Shift" }, "m", function () exec(mail_cmd) end),
    awful.key({ modkey, "Shift" }, "r", function () exec("emacsclient --eval '(make-remember-frame)'") end),
    awful.key({ modkey, "Shift" }, "s", function () scratch.drop(terminal, "center", "center", 0.6, 0.6) end),
    awful.key({ modkey, "Shift" }, "Return", function () exec(terminal) end),
    awful.key({ modkey, "Control" }, "l", function () exec(lock_cmd) end),
    -- awful.key({ modkey }, "g", function () sexec("GTK2_RC_FILES=~/.gtkrc-gajim gajim") end),
    awful.key({ modkey }, "Print", function () exec("scrot -e 'mv $f ~/Images/ 2>/dev/null'") end),
    -- }}}

    -- {{{ Multimedia keys
    awful.key({}, "XF86AudioLowerVolume", function () exec("amixer -q set "..volchan.." 2dB-") end),
    awful.key({}, "XF86AudioRaiseVolume", function () exec("amixer -q set "..volchan.." 2dB+") end),
    awful.key({}, "XF86AudioMute",        function () exec("amixer -q set "..volchan.." toggle") end),
    awful.key({}, "XF86AudioNext",        function() sexec("cmus-remote --next") end ),
    awful.key({}, "XF86AudioPrev",        function() sexec("cmus-remote --prev") end ),
    awful.key({}, "XF86AudioPlay",        function() sexec("cmus-remote --pause") end ),
    awful.key({}, "XF86ScreenSaver",      function() exec(lock_cmd) end ), --fn - f2
    awful.key({}, "XF86Sleep",            function() sexec("sudo pm-suspend") end ), --fn - f4
    -- }}}

    -- {{{ Prompt menus
    -- awful.key({ modkey }, "F2", function ()
    --     awful.prompt.run({ prompt = "Run: " }, mypromptbox[mouse.screen].widget,
    --         function (...) mypromptbox[mouse.screen].text = exec(unpack(arg), false) end,
    --         awful.completion.shell, awful.util.getdir("cache") .. "/history")
    -- end),
    awful.key({ modkey }, "F2", function ()
                 awful.util.spawn("dmenu_run -i -p 'Run command:'" ..
                                  " -nb '" .. beautiful.bg_normal ..
                                  "' -nf '" .. beautiful.fg_normal ..
                                  "' -sb '" .. beautiful.bg_focus ..
                                  "' -sf '" .. beautiful.fg_focus .. "'")
                               end),

    -- awful.key({ modkey }, "F3", function ()
    --     awful.prompt.run({ prompt = "Dictionary: " }, promptbox[mouse.screen].widget,
    --         function (words)
    --             sexec("crodict "..words.." | ".."xmessage -timeout 10 -file -")
    --         end)
    -- end),
    awful.key({ modkey }, "F4", function ()
        awful.prompt.run({ prompt = "Web: " }, promptbox[mouse.screen].widget,
            function (command)
                sexec(browser.." 'http://duckduckgo.com/?q="..command.."'")
                awful.tag.viewonly(tags[scount][2])
            end)
    end),
    awful.key({ modkey }, "F5", function ()
        awful.prompt.run({ prompt = "Lua: " }, promptbox[mouse.screen].widget,
        awful.util.eval, nil, awful.util.getdir("cache") .. "/history_eval")
    end),
    -- }}}

    -- {{{ Awesome controls
    awful.key({ modkey }, "b", function ()
		wibox[mouse.screen].visible = not wibox[mouse.screen].visible
	end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Control" }, "q", awesome.quit),
    -- }}}

    -- {{{ Tag browsing
    awful.key({ modkey }, "Left",   awful.tag.viewprev),
    awful.key({ modkey }, "Right",  awful.tag.viewnext),
    awful.key({ modkey }, "Escape", awful.tag.history.restore),
    -- }}}

    -- {{{ Layout manipulation
    awful.key({ modkey            }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey            }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),
    awful.key({ modkey            }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Control" }, "space", function () awful.layout.set(layouts[1]) end),
    -- }}}

    -- {{{ Focus controls
    -- awful.key({ modkey }, "e", revelation.revelation),
    awful.key({ modkey }, "p", function () awful.screen.focus_relative(1)  end),
    awful.key({ modkey }, "s", function () scratch.pad.toggle() end),
    awful.key({ modkey }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey }, "j", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "k", function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "Tab", function ()
        -- awful.client.focus.history.previous()
        awful.client.cycle(true)
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey, "Shift"   }, "Tab", function ()
        awful.client.cycle(false)
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ altkey }, "Escape", function ()
        awful.menu.menu_keys.down = { "Down", "Alt_L" }
        local cmenu = awful.menu.clients({width=230}, { keygrabber=true, coords={x=525, y=330} })
    end),
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1) end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1) end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end)
    -- }}}
)
-- }}}

-- {{{ Client manipulation
clientkeys = awful.util.table.join(
    awful.key({ modkey }, "c", function (c) c:kill() end),
    awful.key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end),
    awful.key({ modkey }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
    end),
    awful.key({ modkey }, "n", function (c) c.minimized = not c.minimized end),
    awful.key({ modkey }, "o", awful.client.movetoscreen),
    awful.key({ modkey }, "r", function (c) c:redraw() end),
    awful.key({ modkey }, "t", function (c) c.ontop = not c.ontop end),
    awful.key({ modkey }, "Return",  function (c) c:swap(awful.client.getmaster()) end),

    awful.key({ modkey, "Control" }, "s", function (c) scratch.pad.set(c, 0.60, 0.60, true) end),

    -- move and resize floaters with the keyboard
    awful.key({ modkey, "Control" }, "Next",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    awful.key({ modkey, "Control" }, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ modkey, "Control" }, "Down",  function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ modkey, "Control" }, "Up",    function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ modkey, "Control" }, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ modkey, "Control" }, "Right", function () awful.client.moveresize( 20,   0,   0,   0) end),
    -- awful.key({ modkey, "Shift" }, "c", function (c) exec("kill -CONT " .. c.pid) end),
    -- awful.key({ modkey, "Shift" }, "s", function (c) exec("kill -STOP " .. c.pid) end),
    awful.key({ modkey, "Shift" }, "0", function (c) c.sticky = not c.sticky end),
    awful.key({ modkey, "Shift" }, "t", function (c)
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, { modkey = modkey }) end
    end),
    -- floating windows
    awful.key({ modkey, "Control" }, "f", awful.client.floating.toggle),
    awful.key({ modkey, "Shift"   }, "f", function (c) if awful.client.floating.get(c)
        then awful.client.floating.delete(c)
        else awful.client.floating.set(c, true) end
    end)
)
-- }}}

-- {{{ Keyboard digits
-- Compute the maximum number of digit we need, limited to 9
local keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber))
end
-- }}}

-- {{{ Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
        function ()
            local screen = mouse.screen
            if tags[screen][i] then
                awful.tag.viewonly(tags[screen][i])
            end
        end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
        function ()
            local screen = mouse.screen
            if tags[screen][i] then
                awful.tag.viewtoggle(tags[screen][i])
            end
        end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
        function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.movetotag(tags[client.focus.screen][i])
            end
        end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
        function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.toggletag(tags[client.focus.screen][i])
            end
        end))
end
-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}


-- {{{ Rules
-- class: second value of the WM_CLASS property
-- instance: first value of the WM_CLASS property
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = {
          focus = awful.client.focus.filter,
          size_hints_honor = false,
          keys = clientkeys,
          buttons = clientbuttons,
          border_width = beautiful.border_width,
          border_color = beautiful.border_normal
      },
      callback = awful.titlebar.add
    },
    { rule = { class = "Firefox", instance = "Navigator" },
      properties = { tag = tags[screen.count()][2] }, callback = maximize },
    { rule = { class = "Firefox" }, except = { instance = "Navigator" },
      properties = {floating = true}, callback = awful.placement.centered },
    { rule = { class = "URxvt", instance = "htop" },
      properties = { floating = true }, callback = awful.titlebar.remove },
    { rule = { class = "URxvt", instance = "Mutt" },
      properties = { tag = tags[screen.count()][1] }, },
    -- { rule = { class = "Emacs",    instance = "emacs" },
    --   properties = { tag = tags[screen.count()][2] } },
    { rule = { class = "Emacs",    instance = "_Remember_" },
      properties = { floating = true }, callback = awful.titlebar.remove },
    { rule = { class = "Xmessage", instance = "xmessage" },
      properties = { floating = true } },
    { rule = { class = "Thunderbird" }, properties = { tag = tags[1][1]} },
    { rule = { class = "Gajim.py" },    properties = { tag = tags[1][1]} },
    { rule = { class = "gimp" },        properties = { floating = true } },
    -- float & centered
    { rule_any = { class = { "Mirage", "feh", "Pinentry.*" } },
      properties = { floating = true }, callback = awful.placement.centered },
    -- float, centered & no titlebar
    { rule_any = { class = { "MPlayer", "Vlc" } },
      properties = { floating = true }, callback = function(c)
          if c.titlebar then awful.titlebar.remove(c) end
          awful.placement.centered(c)
      end },
}
-- }}}


-- {{{ Signals
--
-- {{{ Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })
    -- Add titlebar to floaters, but remove those from rule callback
    -- if awful.client.floating.get(c)
    -- or awful.layout.get(c.screen) == awful.layout.suit.floating then
    --     if   c.titlebar then awful.titlebar.remove(c)
    --     else awful.titlebar.add(c, {modkey = modkey}) end
    -- end

    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    -- Client placement
    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local title = awful.titlebar.widget.titlewidget(c)
        title:buttons(awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                ))

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(title)

        awful.titlebar(c):set_widget(layout)
    end
end)
-- }}}

-- {{{ Focus signal handlers
client.connect_signal("focus",   function (c) c.border_color = beautiful.border_focus  end)
client.connect_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, scount do screen[s]:connect_signal("arrange", function ()
    local clients = awful.client.visible(s)
    local layout = awful.layout.getname(awful.layout.get(s))

    for _, c in pairs(clients) do -- Floaters are always on top
        if   awful.client.floating.get(c) or layout == "floating"
        then if not c.fullscreen then c.above       =  true  end
        else                          c.above       =  false end
    end
  end)
end
-- }}}
-- }}}

-- {{{ Autostart apps

if host == "thunderball" then
   -- run_once("/usr/lib/gnome-settings-daemon/gnome-settings-daemon")
   -- run_once("nm-applet --sm-disable", "", "nm-applet")
   -- run_once("wicd-client","-t","/usr/bin/python2 -O /usr/share/wicd/gtk/wicd-client.py -t")
   run_once("urxvtd", "-q -f -o", "urxvtd -q -f -o")
   run_once("volumeicon")
   run_once("xcompmgr")
   run_once("conky -c /home/simon/.conky/conkyrc_seamod")
   run_once("syndaemon -t -k -i 2 -d")
   run_once("synapse", "-s", "synapse -s")
   -- run_once("gpaste")
elseif host == "DSK000977" then
   run_once("urxvtd", "-q -f -o", "urxvtd -q -f -o")
   run_once("volumeicon")
   run_once("xcompmgr")
   -- run_once("conky -c /home/simon/.conky/conkyrc_seamod")
   run_once("syndaemon -t -k -i 2 -d")
   -- run_once("synapse", "-s", "synapse -s")
end

-- run_once("xbindkeys")
run_once("xscreensaver -nosplash", nil, "xscreensaver -nosplash")
-- run_once("emacs", "--daemon")

-- }}}
