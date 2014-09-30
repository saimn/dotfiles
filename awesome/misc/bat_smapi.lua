---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- {{{ Grab environment
local util = require("awful.util")
local tonumber = tonumber
local setmetatable = setmetatable
local string = { format = string.format }
local helpers = require("vicious.helpers")
local math = {
    min = math.min,
    floor = math.floor
}
-- }}}

-- Bat_smapi: provides state, charge, remaining time and power consumption information for a requested battery
module("misc.bat_smapi")


-- {{{ Battery widget type
local function worker(format, warg)
    if not warg then return end
    
    if util.file_readable("/sys/devices/platform/smapi/" .. warg .. "/installed") then
	local battery = helpers.pathtotable("/sys/devices/platform/smapi/"..warg)
	local battery_state = {
	["full\n"]        = "↯",
	["idle\n"]     	  = "⌁",
	["none\n"]	  = "",
	["charged\n"]     = "↯",
	["charging\n"]    = "+",
	["discharging\n"] = "-"
	}

	-- Check if the battery is present
	if battery.installed ~= "1\n" then
		return {battery_state["none\n"], "", "n/a", ""}
	end

	-- Get state information
	local state = battery_state[battery.state] or battery_state["idle\n"]
	
	local percent = battery.remaining_percent
	percent = percent:match("(.*)\n$")

	local power = battery.power_now
	power = string.format("%.1f", tonumber(power)/1000)

	local time
	if state == "-" then
		time = battery.remaining_running_time
	elseif state == "+" then
		time = battery.remaining_charging_time
	else
		return {state, percent, "idle", power}
	end
	local hoursleft = math.floor(time / 60)
	local minutesleft = math.floor(time - (hoursleft * 60))
	time = string.format("%02d:%02d", hoursleft, minutesleft)
 
	return {state, percent, time, power}
    else
	local battery = helpers.pathtotable("/sys/class/power_supply/"..warg)
	local battery_state = {
	    ["Full\n"]        = "↯",
	    ["Unknown\n"]     = "⌁",
	    ["Charged\n"]     = "↯",
	    ["Charging\n"]    = "+",
	    ["Discharging\n"] = "-"
	}

	-- Check if the battery is present
	if battery.present ~= "1\n" then
	    return {battery_state["Unknown\n"], 0, "N/A"}
	end


	-- Get state information
	local state = battery_state[battery.status] or battery_state["Unknown\n"]

	-- Get capacity information
	if battery.charge_now then
	    remaining, capacity = battery.charge_now, battery.charge_full
	elseif battery.energy_now then
	    remaining, capacity = battery.energy_now, battery.energy_full
	else
	    return {battery_state["Unknown\n"], 0, "N/A"}
	end

	-- Calculate percentage (but work around broken BAT/ACPI implementations)
	local percent = math.min(math.floor(remaining / capacity * 100), 100)

	-- Get charge information
	if battery.current_now then
	    rate = battery.current_now
	elseif battery.power_now then
	    rate = battery.power_now
	else
	    return {state, percent, "N/A"}
	end
    
	--local watt =  string.format("%d.%d%dW",rate:match("(%d)(%d)(%d)(%d)(%d)(%d)(%d)"))
	local watt = math.min(math.floor(rate / 1000))
    
	-- Calculate remaining (charging or discharging) time
	local time = "N/A"
	if rate ~= nil then
	    if state == "+" then
		timeleft = (tonumber(capacity) - tonumber(remaining)) / tonumber(rate)
	    elseif state == "-" then
		timeleft = tonumber(remaining) / tonumber(rate)
	    else
		return {state, percent, time}
	    end
	    local hoursleft = math.floor(timeleft)
	    local minutesleft = math.floor((timeleft - hoursleft) * 60 )
	    time = string.format("%02d:%02d", hoursleft, minutesleft)
	end

	if state == "-" then
	  return {state, percent, time, watt}
	else
	    return {state, percent, time, "n/a"}
	end
    end
end
-- }}}

setmetatable(_M, { __call = function(_, ...) return worker(...) end })
