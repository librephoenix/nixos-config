-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v2.5/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@type ChadrcConfig
local M = {}

M.base46 = {
	theme = "stylix",

	-- hl_override = {
	-- 	Comment = { italic = true },
	-- 	["@comment"] = { italic = true },
	-- },
}

M.ui = {
 theme = "stylix",
}

M.nvdash = {
 load_on_startup = true,
 buttons = {
   { txt = "  Find File", keys = "Spc .", cmd = "Telescope find_files" },
   { txt = "󰙅  Nvimtree", keys = "SPC e", cmd = "NvimTreeToggle" },
   { txt = "  Projects ", keys = "Spc p p", cmd = "Telescope workspaces" },
   { txt = "󰯌  Vsplit ", keys = "Spc w v", cmd = "vsplit" },
   { txt = "󰯋  Split ", keys = "Spc w s", cmd = "split" },
   { txt =  "󰋗  Help", keys = "Spc c h", cmd = "NvCheatsheet" },
 },
 }

return M
