-- ec = require("editorconfig_core")

local function getProperties(fullpath)
   -- TODO: Avoid command injection vulnerability
   -- For example, following command will create file b.txt
   -- $ micro "a.txt'; touch 'b.txt"
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

local function setIndentation(properties, view)
   local indent_size_str = properties["indent_size"]
   local tab_width_str = properties["tab_width"]
   local indent_style = properties["indent_style"]

   local indent_size = tonumber(indent_size, 10)
   if indent_size ~= nil then
      SetLocalOption("tabsize", indent_size, view)
   end

   if indent_style == "space" then
      messenger:Message("indent_style to space")
      SetLocalOption("indentchar", " ", view)
   elseif indent_style == "tab" then
      messenger:Message("indent_style to tab")
      SetLocalOption("indentchar", "\t", view)
   else
      messenger:Message("unknown indent_style")
   end
end

function onViewOpen(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)
   local properties = getProperties(fullpath)
   if properties["indent_style"] == nil then
      messenger:Message("edconf: nil")
   else
      messenger:Message("edconf: " .. properties["indent_style"])
   end

   setIndentation(properties, view)
   -- setCodingSystem(propertieps, view)
end

function onSave(view)
   messenger:Message("Saved!")
end

-- function getindentstyle()
