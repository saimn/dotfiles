-- {{{ License
--
-- Awesome configuration, based on :
--   * Adrian C. <anrxc@sysphere.org>
--     Screenshot: http://sysphere.org/gallery/snapshots

-- This work is licensed under the Creative Commons Attribution-Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}


-- {{{ Libraries
require("awful")
require("awful.rules")
require("awful.autofocus")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- User libraries
require("functions")
require("vicious")
require("vicious.contrib")
require("scratch")
require("revelation")
-- }}}

-- {{{ Variable definitions
local altkey = "Mod1"
local modkey = "Mod4"

local home   = os.getenv("HOME")
local host   = oscapture("hostname")
local exec   = awful.util.spawn
local sexec  = awful.util.spawn_with_shell
local terminal   = "urxvtc"
local browser    = os.getenv("BROWSER") or "firefox"
local mail_cmd   = terminal.." -T Mutt -name Mutt -e mutt"
local editor     = os.getenv("EDITOR") or "vim"
local editor_cmd = terminal .. " -e " .. editor
local filemgr    = "pcmanfm"
local htop_cmd   = terminal.." -name htop -geometry 80x7 -e htop"

-- Beautiful theme
beautiful.init(home .. "/.config/awesome/themes/zenburn/theme.lua")

-- Window management layouts
layouts = {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.fair,
   awful.layout.suit.spiral,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier
}
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
   { "edit config", editor_cmd.." "..awful.util.getdir("config").."/rc.lua" }
}

mysystemmenu = {
   { "shutdown", awful.util.getdir("config") .. "/scripts/shutdown.sh" },
   { "reboot", awful.util.getdir("config") .. "/scripts/reboot.sh" },
   { "suspend", awful.util.getdir("config") .. "/scripts/suspend.sh" },
   { "hibernate", awful.util.getdir("config") .. "/scripts/hibernate.sh" }
}

mymainmenu = awful.menu({ items = {
                              { "awesome", myawesomemenu, beautiful.awesome_icon },
                              { "system", mysystemmenu },
                              { "term", terminal },
                              { "htop", htop_cmd },
                              { "browser", browser },
                              { "mail", mail_cmd },
                              { "files", filemgr },
                              { "lock", "lock" },
                              { "restart", awesome.restart },
                              { "quit", awesome.quit }
                           }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}


-- {{{ Wibox
--
-- {{{ Widgets configuration
--
-- {{{ Reusable separator
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage and temperature
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph   = awful.widget.graph()
tzswidget  = widget({ type = "textbox" })
loadwidget = widget({ type = "textbox" })
-- Graph properties
cpugraph:set_width(40):set_height(14)
cpugraph:set_background_color(beautiful.fg_off_widget)
cpugraph:set_gradient_angle(0):set_gradient_colors({
   beautiful.fg_end_widget, beautiful.fg_center_widget, beautiful.fg_widget
})
-- Register widgets
vicious.register(cpugraph,   vicious.widgets.cpu,      "$1")
vicious.register(tzswidget,  vicious.widgets.thermal, " $1°C", 19, "thermal_zone0")
-- vicious.register(loadwidget, vicious.widgets.uptime, " $4/$5")
-- Register buttons
cpugraph.widget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec(htop_cmd) end)
))
-- }}}

-- {{{ Battery state
baticon = widget({ type = "imagebox" })
baticon.image = image(beautiful.widget_bat)
-- Initialize widget
batwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(batwidget, vicious.widgets.bat, "$1$2%", 61, "BAT0")
-- }}}

-- {{{ Memory usage
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(16):set_width(8):set_ticks_size(2)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- Register buttons
membar.widget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec(htop_cmd) end)
))
-- }}}

-- {{{ File system usage
fsicon = widget({ type = "imagebox" })
fsicon.image = image(beautiful.widget_fs)
-- Initialize widgets
fs = { r = awful.widget.progressbar(), h = awful.widget.progressbar(),
       d = awful.widget.progressbar() }
-- Progressbar properties
for _, w in pairs(fs) do
  w:set_vertical(true):set_ticks(true)
  w:set_height(16):set_width(6):set_ticks_size(2)
  w:set_border_color(beautiful.border_widget)
  w:set_background_color(beautiful.fg_off_widget)
  w:set_gradient_colors({ beautiful.fg_widget,
     beautiful.fg_center_widget, beautiful.fg_end_widget
  }) -- Register buttons
  w.widget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () exec(filemgr, false) end)
  ))
end -- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",     599)
vicious.register(fs.h, vicious.widgets.fs, "${/home used_p}", 599)
vicious.register(fs.d, vicious.widgets.fs, "${/data used_p}", 599)
-- }}}

-- {{{ Network usage
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image(beautiful.widget_net)
upicon.image = image(beautiful.widget_netup)
-- Initialize widget
netwidget = widget({ type = "textbox" })
if host == "goudes" then netv = "wlan0" else netv = "eth0" end
-- Register widget
vicious.register(netwidget, vicious.widgets.net, '<span color="'
  .. beautiful.fg_netdn_widget ..'">${'.. netv ..' down_kb}</span> <span color="'
  .. beautiful.fg_netup_widget ..'">${'.. netv ..' up_kb}</span>', 3)
-- }}}

-- {{{ Mail subject
mailicon = widget({ type = "imagebox" })
mailicon.image = image(beautiful.widget_mail)
-- Initialize widget
mailwidget = widget({ type = "textbox" })
-- Register widget
maildirs = {home .. "/Mail/INBOX/", home .. "/Mail/INBOX2/"}
vicious.register(mailwidget, vicious.widgets.mdir, "$1 / $2", 301, maildirs)
-- Register buttons
mailwidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec(mail_cmd) end)
))
-- }}}

-- {{{ Org-mode agenda
orgicon = widget({ type = "imagebox" })
orgicon.image = image(beautiful.widget_org)
-- Initialize widget
orgwidget = widget({ type = "textbox" })
-- Configure widget
local orgdir = home.."/org"
local orgmode = {
  files = { orgdir.."/agenda.org", orgdir.."/home.org", orgdir.."/work.org" },
  color = {
    past   = '<span color="'..beautiful.fg_urgent..'">',
    today  = '<span color="'..beautiful.fg_normal..'">',
    soon   = '<span color="'..beautiful.fg_widget..'">',
    future = '<span color="'..beautiful.fg_netup_widget..'">'
}} -- Register widget
vicious.register(orgwidget, vicious.widgets.org,
  orgmode.color.past..'$1</span>-'..orgmode.color.today .. '$2</span>-' ..
  orgmode.color.soon..'$3</span>-'..orgmode.color.future.. '$4</span>', 601,
  orgmode.files
) -- Register buttons
orgwidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec("emacsclient --eval '(org-agenda-list)'") end),
  awful.button({ }, 3, function () exec("emacsclient --eval '(make-remember-frame)'") end)
))
-- }}}

-- {{{ MPD
mpdicon = widget({ type = "imagebox" })
mpdicon.image = image(beautiful.widget_music)
-- Initialize widget
mpdwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(mpdwidget, vicious.widgets.mpd,
    function (widget, args)
        if args["{state}"] == "Stop" then
            return " - "
        else
            return ' '..args["{Artist}"]..' - '.. args["{Title}"]..' '
        end
    end, 10)
-- }}}

-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
-- Progressbar properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_height(16):set_width(8):set_ticks_size(2)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
})
-- Enable caching
-- vicious.cache(vicious.widgets.volume)

-- -- Register widgets
-- if host == "fireball" then volchan = "Master -c 0" else volchan = "Master" end
-- vicious.register(volbar,    vicious.widgets.volume,  "$1",  2, volchan)
-- vicious.register(volwidget, vicious.widgets.volume, " $1%", 2, volchan)
-- -- Register buttons
-- volbar.widget:buttons(awful.util.table.join(
--    awful.button({ }, 1, function () exec(terminal.." -e alsamixer") end),
--    awful.button({ }, 4, function () exec("amixer -q set "..volchan.." 2dB+", false) end),
--    awful.button({ }, 5, function () exec("amixer -q set "..volchan.." 2dB-", false) end)
-- ))

-- Register widgets
volchan = "alsa_output.pci-0000_00_1b.0.analog-stereo"
vicious.register(volbar,    vicious.contrib.pulse, " $1",  2, volchan)
-- vicious.register(volwidget, vicious.contrib.pulse, " $1%", 2, volchan)
-- Register buttons
volbar.widget:buttons(awful.util.table.join(
   awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end),
   awful.button({ }, 4, function () vicious.contrib.pulse.add(5, volchan) end),
   awful.button({ }, 5, function () vicious.contrib.pulse.add(-5, volchan) end)
  ))

-- Register assigned buttons
volwidget:buttons(volbar.widget:buttons())
-- }}}

-- {{{ Weather widget
-- Initialize widget
weathericon = widget({ type = "imagebox" })
weathericon.image = image(beautiful.widget_weather)
weatherwidget = widget({ type = "textbox" })
-- Register widget
-- http://weather.noaa.gov/pub/data/observations/metar/decoded/LFML.TXT
-- vicious.register(weatherwidget, vicious.widgets.weather, "${tempc}° ${wind}, ${windkmh}km/h, ${weather}, ${sky}", 3601, "LFML")
vicious.register(weatherwidget, vicious.widgets.weather, "${tempc}° ${wind}, ${windkmh}km/h", 3601, "LFML")
-- }}}

-- {{{ Date and time
dateicon = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
-- Initialize widget
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%d %b - %R ", 61)
-- Register buttons
datewidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec("emacsclient -c --eval '(org-agenda-list)'") end),
  awful.button({ }, 3, function () exec("emacsclient -c --eval '(make-remember-frame)'") end)
  -- awful.button({ }, 1, function () exec("pylendar.py") end)
))
-- }}}

-- {{{ System tray
systray = widget({ type = "systray" })
-- }}}
-- }}}

-- {{{ Wibox initialisation
wibox     = {}
promptbox = {}
layoutbox = {}
taglist   = {}
taglist.buttons = awful.util.table.join(
    awful.button({ },        1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ },        3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ },        4, awful.tag.viewnext),
    awful.button({ },        5, awful.tag.viewprev)
)
tasklist = {}
tasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
                           if not c:isvisible() then
                              awful.tag.viewonly(c:tags()[1])
                           end
                           client.focus = c
                           c:raise()
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

for s = 1, screen.count() do
    -- Create a promptbox
    promptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create a layoutbox
    layoutbox[s] = awful.widget.layoutbox(s)
    layoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    ))

    -- Create a taglist widget
    taglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, taglist.buttons)

    -- Create a tasklist widget
    tasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, tasklist.buttons)

    -- Create the wibox
    wibox[s] = awful.wibox({      screen = s,
        fg = beautiful.fg_normal, height = 18,
        bg = beautiful.bg_normal, position = "top",
        border_color = beautiful.border_focus,
        border_width = beautiful.border_width
    })
    -- Add widgets to the wibox
    wibox[s].widgets = {
        {
           mylauncher,
           taglist[s],
           layoutbox[s],
           separator,
           promptbox[s],
           ["layout"] = awful.widget.layout.horizontal.leftright
        },
        s == screen.count() and systray or nil,
        separator, datewidget, dateicon,
        separator, weatherwidget, weathericon,
        separator, volwidget, volbar.widget, volicon,
        separator, orgwidget,  orgicon,
        separator, mailwidget, mailicon,
        separator, upicon, netwidget, dnicon,
        separator, fs.r.widget, fs.h.widget, fs.d.widget, fsicon,
        separator, membar.widget, memicon,
        separator, batwidget, baticon,
        separator, loadwidget, tzswidget, cpugraph.widget, cpuicon,
        -- separator, mpdwidget, mpdicon,
        separator, tasklist[s],
        ["layout"] = awful.widget.layout.horizontal.rightleft
    }
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
    -- awful.key({ modkey }, "g", function () sexec("GTK2_RC_FILES=~/.gtkrc-gajim gajim") end),
    awful.key({ modkey }, "Print", function () exec("scrot -e 'mv $f ~/Images/screenshots/ 2>/dev/null'") end),
    -- }}}

    -- {{{ Multimedia keys
    -- awful.key({}, "#121", function () exec("pvol.py -m") end),
    -- awful.key({}, "#122", function () exec("pvol.py -p -c -2") end),
    -- awful.key({}, "#123", function () exec("pvol.py -p -c 2")  end),
    -- awful.key({}, "#232", function () exec("plight.py -s") end),
    -- awful.key({}, "#233", function () exec("plight.py -s") end),
    -- awful.key({}, "#244", function () exec("sudo /usr/sbin/pm-hibernate") end),
    -- awful.key({}, "#150", function () exec("sudo /usr/sbin/pm-suspend")   end),
    -- awful.key({}, "#225", function () exec("pypres.py") end),
    -- awful.key({}, "#157", function () if boosk then osk()
    --     else boosk, osk = pcall(require, "osk") end
    -- end),
    -- }}}

    -- {{{ Prompt menus
    awful.key({ modkey,}, "w", function () mymainmenu:show({keygrabber=true}) end),
    -- awful.key({ modkey }, "F2", function ()
    --     awful.prompt.run({ prompt = "Run: " }, promptbox[mouse.screen].widget,
    --         function (...) promptbox[mouse.screen].text = exec(unpack(arg), false) end,
    --         awful.completion.shell, awful.util.getdir("cache") .. "/history")
    -- end),
    awful.key({ modkey }, "F2", function ()
         exec("dmenu_run -fn '-*-terminus-*-r-normal-*-*-160-*-*-*-*-iso8859-*' -nb '#3F3F3F' -nf '#DCDCCC' -sb '#1E2320' -sf '#CC9393'")
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
                awful.tag.viewonly(tags[screen.count()][2])
            end)
    end),
    awful.key({ modkey }, "F5", function ()
        awful.prompt.run({ prompt = "Lua: " }, promptbox[mouse.screen].widget,
        awful.util.eval, nil, awful.util.getdir("cache") .. "/history_eval")
    end),
    -- }}}

    -- {{{ Awesome controls
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Control" }, "q", awesome.quit),
    -- awful.key({ modkey, "Shift" }, "r", function ()
    --     promptbox[mouse.screen].text = awful.util.escape(awful.util.restart())
    -- end),
    -- }}}

    -- {{{ Tag browsing
    awful.key({ modkey }, "Left",   awful.tag.viewprev),
    awful.key({ modkey }, "Right",  awful.tag.viewnext),
    awful.key({ modkey }, "Escape", awful.tag.history.restore),
    -- }}}

    -- {{{ Layout manipulation
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.02)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.02)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),
    awful.key({ modkey, "Control" }, "space", function () awful.layout.set(layouts[1]) end),
    -- }}}

    -- {{{ Focus controls
    awful.key({ modkey }, "e",  revelation.revelation),
    awful.key({ modkey }, "j", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "k", function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey, "Shift"   }, "j",     function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k",     function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j",     function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k",     function () awful.screen.focus_relative(-1) end),

    awful.key({ modkey            }, "p",     function () awful.screen.focus_relative(1)  end),
    awful.key({ modkey            }, "s",     function () scratch.pad.toggle()            end),
    awful.key({ modkey            }, "u",     awful.client.urgent.jumpto),

    awful.key({ modkey,           }, "Tab", function ()
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
    end)
    -- }}}
)
-- }}}

-- {{{ Client manipulation
clientkeys = awful.util.table.join(
    awful.key({ modkey }, "b", function () wibox[mouse.screen].visible = not wibox[mouse.screen].visible end),
    awful.key({ modkey }, "c", function (c) c:kill() end),
    awful.key({ modkey }, "d", function (c) scratch.pad.set(c, 0.60, 0.60, true) end),
    awful.key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end),
    awful.key({ modkey }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
        -- if   c.titlebar then awful.titlebar.remove(c)
        -- else awful.titlebar.add(c, { modkey = modkey }) end
    end),
    awful.key({ modkey }, "n", function (c) c.minimized = not c.minimized end),
    awful.key({ modkey }, "o", awful.client.movetoscreen),
    awful.key({ modkey }, "r", function (c) c:redraw() end),
    awful.key({ modkey }, "t", function (c) c.ontop = not c.ontop end),
    awful.key({ modkey }, "Return",  function (c) c:swap(awful.client.getmaster()) end),
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
        then awful.client.floating.delete(c);    awful.titlebar.remove(c)
        else awful.client.floating.set(c, true); awful.titlebar.add(c) end
    end)
)
-- }}}

-- {{{ Keyboard digits
local keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end
-- }}}

-- {{{ Tag controls
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join( globalkeys,
        awful.key({ modkey }, "#" .. i + 9, function ()
            local screen = mouse.screen
            if tags[screen][i] then awful.tag.viewonly(tags[screen][i]) end
        end),
        awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
            local screen = mouse.screen
            if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
        end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.movetotag(tags[client.focus.screen][i])
            end
        end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
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
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = {
         focus = true,
         size_hints_honor = false,
         keys = clientkeys,
         buttons = clientbuttons,
         maximized_vertical   = false,
         maximized_horizontal = false,
         border_width = beautiful.border_width,
         border_color = beautiful.border_normal },
      callback = awful.titlebar.add
    },
    { rule = { class = "Firefox",  instance = "Navigator" },
      properties = { tag = tags[screen.count()][2], floating = true },
      callback = awful.titlebar.remove },
    { rule = { class = "URxvt",  instance = "htop" },
      properties = { floating = true }, callback = awful.titlebar.remove },
    { rule = { class = "URxvt",  instance = "Mutt" },
      properties = { tag = tags[screen.count()][1] }, },
    -- { rule = { class = "Emacs",    instance = "emacs" },
    --   properties = { tag = tags[screen.count()][2] } },
    { rule = { class = "Emacs",    instance = "_Remember_" },
      properties = { floating = true }, callback = awful.titlebar.remove },
    { rule = { class = "Xmessage", instance = "xmessage" },
      properties = { floating = true }, callback = awful.titlebar.remove },
    { rule = { class = "Thunderbird" }, properties = { tag = tags[1][1]} },
    { rule = { class = "Gajim.py" },    properties = { tag = tags[1][1]} },
    { rule = { class = "gimp" },        properties = { floating = true } },
    -- float & centered
    { rule_any = { class = { "Mirage", "feh", "Pinentry.*" } },
      properties = { floating = true }, callback = awful.placement.centered },
    -- float, centered & no titlebar
    { rule_any = { class = { "MPlayer", "Vlc" } },
      properties = { floating = true }, callback = function(c)
                                                      awful.titlebar.remove(c)
                                                      awful.placement.centered(c)
                                                   end },
}
-- }}}


-- {{{ Signals
--
-- {{{ Manage signal handler
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })
    -- Add titlebar to floaters, but remove those from rule callback
    -- if awful.client.floating.get(c)
    -- or awful.layout.get(c.screen) == awful.layout.suit.floating then
    --     if   c.titlebar then awful.titlebar.remove(c)
    --     else awful.titlebar.add(c, {modkey = modkey}) end
    -- else
    --     awful.titlebar.add(c, { modkey = modkey })
    -- end

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function (c)
        if  awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
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
        if  not c.size_hints.program_position
        and not c.size_hints.user_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)
-- }}}

-- {{{ Focus signal handlers
client.add_signal("focus",   function (c) c.border_color = beautiful.border_focus  end)
client.add_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, screen.count() do screen[s]:add_signal("arrange", function ()
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

-- run_once(home .. "/dotfiles/config/autostart.sh")
-- gnome-settings-daemon &
-- nm-applet --sm-disable &
-- gnome-volume-control-applet &
-- gpk-update-icon &
-- emacs --daemon &

if host == "goudes" then
   run_once("/usr/lib/gnome-settings-daemon/gnome-settings-daemon")
   run_once("wicd-client",nil,"/usr/bin/python2 -O /usr/share/wicd/gtk/wicd-client.py")
   run_once("urxvtd", "-q -f -o", "urxvtd -q -f -o")
   run_once("gpaste")
elseif host == "fireball" then
   run_once("/usr/libexec/gnome-settings-daemon")
   run_once("pulseaudio", "--start")
   run_once("urxvt256c-mld", "-q -f -o", "urxvt256c-mld -q -f -o")
   run_once("gpaste", nil, "/usr/libexec/gpaste/gpasted")
end

-- run_once("xbindkeys")
run_once("emacs", "--daemon")

-- }}}
