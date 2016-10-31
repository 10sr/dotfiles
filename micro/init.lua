-- ec = require("editorconfig_core")

function onViewOpen(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)
   local out = getProperties(fullpath)
   messenger:Message("view.Buf.Path: " .. out)
end

local function getProperties(fullpath)
   local file = io.popen("echo fullpath: " .. fullpath, "r")
   local output = file:read("*all")
   file:close()
   return output
end


function onSave(view)
   messenger:Message("Saved!")
end
