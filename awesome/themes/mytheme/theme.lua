-- based on niceandclean, awesome3 theme, by Blazeix

--{{{ Main
local awful = require("awful")
awful.util = require("awful.util")

theme = {}

config        = awful.util.getdir("config")
themes        = config .. "/themes"
themename     = "/mytheme"
themedir      = themes .. themename
theme.wallpaper = themedir .. "/rainbow.png"
--}}}

theme.font          = "Source Sans Pro 10"

theme.bg_normal     = "#222222"
theme.bg_focus      = "#444444"
theme.bg_urgent     = "#d02e54"
theme.bg_minimize   = "#444444"

theme.fg_normal     = "#cccccc"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.border_width  = "3"
theme.border_normal = "#747474"
--theme.border_focus  = "#535d6c"
theme.border_marked = "#91231c"
theme.border_focus  = "#ce2c51"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
-- theme.tasklist_bg_focus = "#1A1A1A"
-- theme.tasklist_fg_focus = "#D8D782"

-- Display the taglist squares
theme.taglist_squares_sel = themedir .. "/taglist/squarefw.png"
theme.taglist_squares_unsel = themedir .. "/taglist/squarew.png"

theme.tasklist_floating_icon = themedir .. "/tasklist/floatingw_grey.png"

theme.vol_bg = themedir .. "/icons/vol_bg.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
-- theme.menu_submenu_icon = themedir .. "/submenu.png"
theme.menu_height = "15"
theme.menu_width  = "110"
theme.menu_border_width = "0"

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themedir .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus = themedir .. "/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = themedir .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive = themedir .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themedir .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active = themedir .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themedir .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive = themedir .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themedir .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active = themedir .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themedir .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive = themedir .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themedir .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active = themedir .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themedir .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive = themedir .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themedir .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active = themedir .. "/titlebar/maximized_focus_active.png"

-- You can use your own layout icons like this:
-- theme.layout_fairh = themedir .. "/layouts/fairhw.png"
-- theme.layout_fairv = themedir .. "/layouts/fairvw.png"
-- theme.layout_floating = themedir .. "/layouts/floatingw.png"
-- theme.layout_magnifier = themedir .. "/layouts/magnifierw.png"
-- theme.layout_max = themedir .. "/layouts/maxw.png"
-- theme.layout_fullscreen = themedir .. "/layouts/fullscreenw.png"
-- theme.layout_tilebottom = themedir .. "/layouts/tilebottomw.png"
-- theme.layout_tileleft = themedir .. "/layouts/tileleftw.png"
-- theme.layout_tile = themedir .. "/layouts/tilew.png"
-- theme.layout_tiletop = themedir .. "/layouts/tiletopw.png"
-- theme.layout_spiral = themedir .. "/layouts/spiralw.png"
-- theme.layout_dwindle = themedir .. "/layouts/dwindlew.png"

theme.layout_tile           = themedir .. "/icons/tile.png"
theme.layout_tilegaps       = themedir .. "/icons/tilegaps.png"
theme.layout_tileleft       = themedir .. "/icons/tileleft.png"
theme.layout_tilebottom     = themedir .. "/icons/tilebottom.png"
theme.layout_tiletop        = themedir .. "/icons/tiletop.png"
theme.layout_fairv          = themedir .. "/icons/fairv.png"
theme.layout_fairh          = themedir .. "/icons/fairh.png"
theme.layout_spiral         = themedir .. "/icons/spiral.png"
theme.layout_dwindle        = themedir .. "/icons/dwindle.png"
theme.layout_max            = themedir .. "/icons/max.png"
theme.layout_fullscreen     = themedir .. "/icons/fullscreen.png"
theme.layout_magnifier      = themedir .. "/icons/magnifier.png"
theme.layout_floating       = themedir .. "/icons/floating.png"

theme.arrl                  = themedir .. "/icons/arrl.png"

theme.widget_ac             = themedir .. "/icons/ac.png"
theme.widget_battery_empty  = themedir .. "/icons/battery_empty.png"
theme.widget_battery_low    = themedir .. "/icons/battery_low.png"
theme.widget_battery        = themedir .. "/icons/bat.png"
theme.widget_clock          = themedir .. "/icons/time.png"
theme.widget_cpu            = themedir .. "/icons/cpuinfow.png"
theme.widget_hdd            = themedir .. "/icons/hdd.png"
theme.widget_mail_on        = themedir .. "/icons/mail_on.png"
theme.widget_mail           = themedir .. "/icons/mail.png"
theme.widget_mem            = themedir .. "/icons/mem.png"
theme.widget_music_on       = themedir .. "/icons/note_on.png"
theme.widget_music          = themedir .. "/icons/note.png"
theme.widget_net_up         = themedir .. "/icons/upw-green.png"
theme.widget_net_down       = themedir .. "/icons/downw.png"
theme.widget_temp           = themedir .. "/icons/temp.png"
theme.widget_vol_low        = themedir .. "/icons/vol_low.png"
theme.widget_vol_mute       = themedir .. "/icons/vol_mute.png"
theme.widget_vol_no         = themedir .. "/icons/vol_no.png"
theme.widget_vol            = themedir .. "/icons/vol.png"

theme.awesome_icon = themedir .. "/awesome16.png"

-- Lain layouts
theme.useless_gap_width  = 15
theme.lain_icons         = config .. "/lain/icons/layout/default/"
theme.layout_termfair    = theme.lain_icons .. "termfairw.png"
theme.layout_cascade     = theme.lain_icons .. "cascadew.png"
theme.layout_cascadetile = theme.lain_icons .. "cascadetilew.png"
theme.layout_centerwork  = theme.lain_icons .. "centerworkw.png"
theme.layout_uselesstile = theme.layout_tile
theme.layout_uselesstileleft = theme.layout_tileleft
theme.layout_uselesstilebottom = theme.layout_tilebottom
theme.layout_uselessfair = theme.layout_fairh

return theme
