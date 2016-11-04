-- ec = require("editorconfig_core")

local function getProperties(fullpath)
   -- TODO: Avoid command injection vulnerability
   local file = io.popen("editorconfig '" .. fullpath .. "'", "r")
   local output = file:read("*all")
   file:close()

   local properties = {}
   -- TODO: Which is better? output:gmatch(), string.gmatch(output, ...)
   for line in output:gmatch('([^\n]+)') do
      -- TODO: Fix regex
      local key, value = line:match('([^=]*)=(.*)')
      key = key:gsub('^%s(.-)%s*$', '%1')
      value = value:gsub('^%s(.-)%s*$', '%1')
      -- TODO: Throw error when key is empty string
      properties[key] = value
   end

   return properties
end

function onViewOpen(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)
   local out = getProperties(fullpath)
   messenger:Message("edconf: " .. out["indent_style"])
end

function onSave(view)
   messenger:Message("Saved!")
end
