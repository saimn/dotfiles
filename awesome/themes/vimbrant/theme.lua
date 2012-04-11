--------------------------------
--  "Vimbrant" awesome theme  --
--    License:  GNU GPL v2    --
--------------------------------

-- {{{ Main
theme = {}
theme.confdir       = awful.util.getdir("config").."/themes/vimbrant"
-- theme.wallpaper_cmd = { "/usr/bin/nitrogen --restore" }
theme.wallpaper_cmd = { "awsetbg -l" }
-- theme.wallpaper_cmd = { "awsetbg /usr/share/awesome/themes/zenburn/zenburn-background.png" }
-- }}}


-- {{{ Styles
-- theme.font      = "Inconsolata 8"
theme.font      = "HeldustryFTVBasic Black 8"

-- {{{ Colors
theme.fg_normal = "#bfbfba"
theme.fg_focus  = "#fd971f"
theme.fg_urgent = "#f92672"
theme.bg_normal = "#1b1d1e"
theme.bg_focus  = "#505354"
theme.bg_urgent = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width  = 1
theme.border_focus  = "#505354"
theme.border_normal = theme.bg_normal
theme.border_marked = theme.fg_urgent
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.bg_normal
theme.titlebar_bg_normal = theme.bg_normal
theme.titlebar_fg_focus  = "#8cedff"
-- theme.titlebar_[normal|focus]
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]

-- {{{ Widgets
theme.fg_widget        = "#82b414"
theme.fg_center_widget = "#b6e354"
theme.fg_end_widget    = "#fd971f"
theme.fg_off_widget    = "#505354"
theme.fg_netup_widget  = "#b6e354"
theme.fg_netdn_widget  = theme.fg_urgent
theme.bg_widget        = theme.bg_normal
theme.border_widget    = theme.bg_normal
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = theme.fg_urgent
-- theme.mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Tooltips
-- theme.tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- }}}

-- {{{ Taglist and Tasklist
-- theme.[taglist|tasklist]_[bg|fg]_[focus|urgent]
-- theme.taglist_bg_focus = theme.bg_normal
theme.tasklist_bg_focus = theme.bg_normal
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "18"
theme.menu_width  = "100"
theme.menu_border_width = 1
theme.menu_border_color = "#505354"
-- }}}
-- }}}


-- {{{ Icons
--
-- {{{ Taglist icons
theme.taglist_squares_sel   = theme.confdir .. "/icons/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.confdir .. "/icons/taglist/squareza.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc icons
theme.awesome_icon           = theme.confdir .. "/icons/awesome.png"
--theme.menu_submenu_icon      = "/usr/share/awesome/themes/default/submenu.png"
--theme.tasklist_floating_icon = "/usr/share/awesome/themes/default/tasklist/floatingw.png"
-- }}}

-- {{{ Layout icons
theme.layout_tile       = theme.confdir .. "/icons/layouts/tile.png"
theme.layout_tileleft   = theme.confdir .. "/icons/layouts/tileleft.png"
theme.layout_tilebottom = theme.confdir .. "/icons/layouts/tilebottom.png"
theme.layout_tiletop    = theme.confdir .. "/icons/layouts/tiletop.png"
theme.layout_fairv      = theme.confdir .. "/icons/layouts/fairv.png"
theme.layout_fairh      = theme.confdir .. "/icons/layouts/fairh.png"
theme.layout_spiral     = theme.confdir .. "/icons/layouts/spiral.png"
theme.layout_dwindle    = theme.confdir .. "/icons/layouts/dwindle.png"
theme.layout_max        = theme.confdir .. "/icons/layouts/max.png"
theme.layout_fullscreen = theme.confdir .. "/icons/layouts/fullscreen.png"
theme.layout_magnifier  = theme.confdir .. "/icons/layouts/magnifier.png"
theme.layout_floating   = theme.confdir .. "/icons/layouts/floating.png"
-- }}}

-- {{{ Widget icons
theme.widget_cpu     = theme.confdir .. "/icons/cpu.png"
theme.widget_bat     = theme.confdir .. "/icons/bat.png"
theme.widget_mem     = theme.confdir .. "/icons/mem.png"
theme.widget_fs      = theme.confdir .. "/icons/disk.png"
theme.widget_net     = theme.confdir .. "/icons/down.png"
theme.widget_netup   = theme.confdir .. "/icons/up.png"
theme.widget_wifi    = theme.confdir .. "/icons/wifi.png"
theme.widget_mail    = theme.confdir .. "/icons/mail.png"
theme.widget_vol     = theme.confdir .. "/icons/vol.png"
theme.widget_music   = theme.confdir .. "/icons/music.png"
theme.widget_org     = theme.confdir .. "/icons/cal.png"
theme.widget_date    = theme.confdir .. "/icons/time.png"
theme.widget_crypto  = theme.confdir .. "/icons/crypto.png"
theme.widget_sep     = theme.confdir .. "/icons/separator.png"
theme.widget_weather = theme.confdir .. "/icons/sun.png"
-- }}}

-- {{{ Titlebar icons
theme.titlebar_close_button_focus  = theme.confdir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.confdir .. "/icons/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active    = theme.confdir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active   = theme.confdir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme.confdir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.confdir .. "/icons/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active    = theme.confdir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active   = theme.confdir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme.confdir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.confdir .. "/icons/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active    = theme.confdir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active   = theme.confdir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme.confdir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.confdir .. "/icons/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active    = theme.confdir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.confdir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "/icons/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}


return theme
