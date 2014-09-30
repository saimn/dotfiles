-- Show fancy notifications for backlight and (eventually) volume hotkeys
require("awful.util")
config      = awful.util.getdir("config")
themedir    = config .. "/themes"
wicons      = themedir .. "/icons"

green = "#a6e22e"-- bg_focus
brown = "#262729"-- bg_normal
blue  = "#34bdef"-- fg_title
red   = "#f92671"-- bg_urgent]]

fg_bar = blue--beautiful.bg_focus or "#a6e22e"
bg_bar = brown--beautiful.bg_normal or "#262729"
bg     = "#262729"--beautiful.bg_normal or "#262729"

function brightness_down()
    brightness_adjust(-1)
end

function brightness_up()
    brightness_adjust(1)
end

function brightness_adjust(inc)
    --local brightness = tonumber(awful.util.pread("xbacklight -get"))
    local brightness = tonumber(awful.util.pread("cat /sys/class/backlight/acpi_video0/brightness"))
    brightness_notify(brightness)
end

local bright_notification = nil
local bright_icon = image(wicons .. "/guff/brightness.png")
local bright_img = image.argb32(212, 50, nil)
bright_img:draw_rectangle(0, 0, bright_img.width, bright_img.height, true,
    bg)
bright_img:insert(bright_icon, 0, 1)

function brightness_notify(brightness)
    local img = bright_img
    img:draw_rectangle(60, 20, 130, 10, true, bg_bar)
    --img:draw_rectangle(62, 22, 126 * brightness / 100, 6, true, fg_bar)
    img:draw_rectangle(62, 22, 126 * brightness / 15, 6, true, fg_bar)
    
    local id = nil
    if bright_notification then id = bright_notification.id end
    bright_notification = naughty.notify(
        { icon = img, replaces_id = id, text = "\n" .. math.ceil(brightness) .. "  ",
          font = "Sans Bold 10" }
    )
end

function volume_down()
    volume_adjust(-5)
end

function volume_up()
    volume_adjust(5)
end

function volume_mute()
    volume_adjust(0)
end

function volume_adjust(inc)
    if inc < 0 then inc = math.abs(inc) .. "%-"
    elseif inc > 0 then inc = inc .. "%+"
    else inc = "toggle" end
    os.execute("amixer set Master " .. inc .. " > /dev/null 2>&1")

    local volume = tonumber(
        awful.util.pread("amixer get Master | grep -om1 '[[:digit:]]*%' | tr -d %")
    )
    local is_muted = string.find(awful.util.pread("amixer get Master"),
                                 '%[on%]') == nil
    if is_muted then volume_notify(0) else volume_notify(volume) end
end

function volume_get_icon()
    local volume = tonumber(
        awful.util.pread("amixer get Master | grep -om1 '[[:digit:]]*%' | tr -d %")
    )
    local is_muted = string.find(awful.util.pread("amixer get Master"),
                                 '%[on%]') == nil
    local icon_str = nil
    if volume > 70 then icon_str = "high.png"
    elseif volume > 30 then icon_str = "medium.png"
    elseif volume > 0 then icon_str = "low.png"
    elseif volume == 0 then icon_str = "off.png" end
    if is_muted then icon_str = "muted.png" end
    return wicons .. "/guff/volume-" .. icon_str
end

local vol_notification = nil
local vol_img = image.argb32(200, 50, nil)

function volume_notify(volume)
    local img = vol_img
    img:draw_rectangle(0, 0, vol_img.width, vol_img.height, true,
        bg)
    local vol_icon = image(volume_get_icon())
    img:insert(vol_icon, 0, 1)
    img:draw_rectangle(60, 20, 130, 10, true, bg_bar)
    img:draw_rectangle(62, 22, 126 * volume / 100, 6, true, fg_bar)
    
    local id = nil
    if vol_notification then id = vol_notification.id end
    vol_notification = naughty.notify(
        { icon = img, replaces_id = id, text = "\n" .. math.ceil(volume) .. "%",
          font = "Sans Bold 10" }
    )
    img = vol_img
end
