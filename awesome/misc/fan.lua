-- {{{ Grab environment
--local io = { popen = io.popen }
local setmetatable = setmetatable
local table = { insert = table.insert }
-- }}}

-- Netcfg: provides active netcfg network profiles
module("fan")

-- {{{ Fan widget type
local function worker(format)

	for line in io.lines("/proc/acpi/ibm/fan")
		if line:match("speed:") then
			speed = print(line:match("%S+%s+(%S+)"))
		end
		return speed
	end
-- }}}

setmetatable(_M, { __call = function(_, ...) return worker(...) end })