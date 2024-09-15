require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set
local builtin = require("telescope.builtin")
local utils = require("telescope.utils")
local change_scale_factor = function(delta)
  vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
end

vim.keymap.set("n", "<C-=>", function()
  change_scale_factor(1.25)
end)
vim.keymap.set("n", "<C-->", function()
  change_scale_factor(1/1.25)
end)

map("n", ";", ":", { desc = "CMD enter command mode" })
map({"n", "v", "i"}, "<A-x>", ":", { desc = "CMD enter command mode" })
map("n", "<leader>.", "<cmd>Telescope find_files<cr>", { desc = "telescope find files" })
map("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "Neogit status buffer" })
map("n", "<leader>pp", "<cmd>Telescope workspaces<cr>", { desc = "telescope workspaces" })
map("i", "jk", "<ESC>")

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
