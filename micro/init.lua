-- ec = require("editorconfig_core")

local function GetProperties(fullpath)
   local file = io.popen("echo fullpath: " .. fullpath, "r")
   local output = file:read("*all")
   file:close()
   return output
end

function onViewOpen(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)
   local out = GetProperties(fullpath)
   messenger:Message("view.Buf.Path: " .. out)
end



function onSave(view)
   messenger:Message("Saved!")
end
