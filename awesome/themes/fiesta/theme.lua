-------------------------------
--  "Fiesta" awesome theme  --
-------------------------------

-- {{{ Main
theme = {}
theme.wallpaper_cmd = { "awsetbg -c "..os.getenv("HOME").."/.config/awesome/themes/fiesta/desktop1.jpg" }
-- }}}

theme.path = os.getenv("HOME").."/.config/awesome/themes/fiesta"

-- {{{ Styles
theme.font      = "Inconsolata 8"

-- {{{ Colors
theme.fg_normal = "#ffffff"
theme.fg_focus  = "#b1d631"
theme.fg_urgent = "#ff5d28"
theme.bg_normal = "#000000"
theme.bg_focus  = "#000099"
theme.bg_urgent = "#000099"
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = "#3f3f3f"
theme.border_focus  = "#ababab"
-- }}}

theme.opacity_focus = 0.85
theme.opacity_normal = 0.75


-- {{{ Widgets
theme.fg_widget        = "#ababab"
theme.fg_center_widget = "#ababab"
theme.fg_end_widget    = "#e8ae5b"
theme.bg_widget = "#1c1c1c"
theme.border_widget = "#4d4d4d"

theme.menu_height = "15"
theme.menu_width  = "100"

theme.taglist_squares_sel   = theme.path.."/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.path.."/taglist/squarez.png"

theme.awesome_icon           = theme.path.."/awesome-icon.png"
theme.menu_submenu_icon      = "/usr/share/awesome/themes/default/submenu.png"
theme.tasklist_floating_icon = "/usr/share/awesome/themes/default/tasklist/floatingw.png"

theme.layout_tile       = theme.path.."/layouts/tile.png"
theme.layout_tileleft   = theme.path.."/layouts/tileleft.png"
theme.layout_tilebottom = theme.path.."/layouts/tilebottom.png"
theme.layout_tiletop    = theme.path.."/layouts/tiletop.png"
theme.layout_fairv      = theme.path.."/layouts/fairv.png"
theme.layout_fairh      = theme.path.."/layouts/fairh.png"
theme.layout_spiral     = theme.path.."/layouts/spiral.png"
theme.layout_dwindle    = theme.path.."/layouts/dwindle.png"
theme.layout_max        = theme.path.."/layouts/max.png"
theme.layout_fullscreen = theme.path.."/layouts/fullscreen.png"
theme.layout_magnifier  = theme.path.."/layouts/magnifier.png"
theme.layout_floating   = theme.path.."/layouts/floating.png"

theme.widget_cpu    = theme.path.."/icons2/cpu.png"
theme.widget_netup  = theme.path.."/icons2/up.png"
theme.widget_fs     = theme.path.."/icons2/disk.png"
theme.widget_vol    = theme.path.."/icons2/vol.png"
theme.widget_date   = theme.path.."/icons2/time.png"
theme.widget_music  = theme.path.."/icons2/music.png"
theme.widget_mem    = theme.path.."/icons2/mem.png"
theme.widget_net    = theme.path.."/icons2/down.png"


return theme
