-- ec = require("editorconfig_core")

local function setIndentation(properties, view)
   local indent_size_str = properties["indent_size"]
   local tab_width_str = properties["tab_width"]
   local indent_style = properties["indent_style"]

   -- TODO: Fix logic to decide indent_size
   local indent_size = tonumber(indent_size_str, 10)
   if indent_size ~= nil then
      messenger:Message("set tabsize to " .. indent_size_str)
      SetLocalOption("tabsize", indent_size, view)
   end

   if indent_style == "space" then
      SetLocalOption("tabstospaces", "on", view)
   elseif indent_style == "tab" then
      SetLocalOption("tabstospaces", "off", view)
   else
      messenger:Message("unknown indent_style")
   end
end

local function setInsertFinalNewline(properties, view)
   local val = properties["insert_final_newline"]
   if val == "true" then
      SetLocalOption("eofnewline", true, view)
   elseif val == "false" then
      SetLocalOption("eofnewline", false, view)
   end
end

function onEditorConfigExit(output)
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

   local view = CurView()

   setIndentation(properties, view)
   -- Currently micro does not support changing coding-systems
   -- (Always use utf-8 with LF?)
   -- setCodingSystem(properties, view)
   -- `ruler' is not what we want!
   -- setMaxLineLength(properties, view)
   -- setTrimTrailingWhitespace(properties, view)
   setInsertFinalNewline(properties, view)
end

local function applyProperties(view)
   -- Is this portable? (work on windows?)
   local pwd = os.getenv("PWD")
   local filename = view.Buf.Path
   -- prop, names = ec.parse(filepath)
   local fullpath = JoinPaths(pwd, filename)

   -- FIXME: Commands can be injected here!
   -- For example, issuing this command will create file b.txt
   -- $ micro "a.txt'; touch 'b.txt"
   JobStart("editorconfig " .. fullpath, "", "", "init.onEditorConfigExit")
end

function onViewOpen(view)
   applyProperties(view)
end

function onSave(view)
   -- messenger:Message("Saved!")
end
