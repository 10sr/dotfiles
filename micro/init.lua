-- ec = require("editorconfig_core")

function onViewOpen(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)
   messenger:Message("view.Buf.Path: " .. fullpath)
end

function onSave(view)
   messenger:Message("Saved!")
end
