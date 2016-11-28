-- ec = require("editorconfig_core")

local function setIndentation(properties, view)
   local indent_size_str = properties["indent_size"]
   local tab_width_str = properties["tab_width"]
   local indent_style = properties["indent_style"]

   -- TODO: Fix logic to decide indent_size
   local indent_size = tonumber(indent_size_str, 10)
   local tab_width = tonumber(tab_width_str, 10)
   if indent_size_str == "tab" then
      indent_size = tab_width
   elseif tab_width == nil then
      tab_width = indent_size
   end

   if indent_size ~= nil then
      messenger:Message("set tabsize to " .. indent_size_str)
      SetLocalOption("tabsize", indent_size, view)
   end

   if indent_style == "space" then
      SetLocalOption("tabstospaces", "on", view)
      if indent_size ~= nil then
         messenger:Message("set tabsize to " .. tostring(indent_size))
         SetLocalOption("tabsize", indent_size, view)
      end
   elseif indent_style == "tab" then
      SetLocalOption("tabstospaces", "off", view)
      if tab_width ~= nil then
         messenger:Message("set tabsize to " .. tostring(tab_width))
         SetLocalOption("tabsize", tab_width, view)
      end
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

local function applyProperties(properties, view)
   setIndentation(properties, view)
   -- Currently micro does not support changing coding-systems
   -- (Always use utf-8 with LF?)
   -- setCodingSystem(properties, view)
   -- `ruler' is not what we want!
   -- setMaxLineLength(properties, view)
   -- setTrimTrailingWhitespace(properties, view)
   setInsertFinalNewline(properties, view)
   -- messenger:Message("ed output: " .. output)
end

function onEditorConfigExit(output)
   -- messenger:Message(output)
   -- FIXME: messege when editorconfig exit with error
   local properties = {}
   -- TODO: Which is better? output:gmatch(), string.gmatch(output, ...)
   for line in output:gmatch('([^\n]+)') do
      -- TODO: Fix regex
      -- TODO: Throw error for invalid output
      local key, value = line:match('([^=]*)=(.*)')
      key = key:gsub('^%s(.-)%s*$', '%1')
      value = value:gsub('^%s(.-)%s*$', '%1')
      properties[key] = value
   end

   local view = CurView()
   applyProperties(properties, view)
end

local function getApplyProperties(view)
   local fullpath = view.Buf.AbsPath
   messenger:Message("editorconfig " .. fullpath)
   -- JobSpawn("editorconfig", {fullpath}, "", "", "init.onEditorConfigExit")
   JobStart("editorconfig " .. fullpath, "", "", "init.onEditorConfigExit")
end

function onViewOpen(view)
   getApplyProperties(view)
   -- messenger:Message(view.Buf.AbsPath)
end

function onSave(view)
   -- messenger:Message("Saved!")
end
