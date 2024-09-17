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
 nvdash = {
   load_on_startup = true,
   buttons = {
     { "  Find File", "Spc .", "Telescope find_files" },
     { "󰙅  Nvimtree", "SPC e", "NvimTreeToggle" },
     { "  Projects ", "Spc p p", "Telescope workspaces" },
     { "󰯌  Vsplit ", "Spc w v", "vsplit" },
     { "󰯋  Split ", "Spc w s", "split" },
     { "󰋗  Help", "Spc c h", "NvCheatsheet" },
   },
 }
}

return M
