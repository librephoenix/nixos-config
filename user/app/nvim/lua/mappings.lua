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
map({"n", "t", "v", "i"}, "<A-x>", ":", { desc = "CMD enter command mode" })
map({"n", "t", "v", "i"}, "<C-n>", "<Down>", { desc = "up" })
map({"n", "t", "v", "i"}, "<C-p>", "<Up>", { desc = "down" })
map({"n", "t", "v", "i"}, "<C-h>", "<C-w>h", { desc = "switch window left" })
map({"n", "t", "v", "i"}, "<C-l>", "<C-w>l", { desc = "switch window right" })
map({"n", "t", "v", "i"}, "<C-j>", "<C-w>j", { desc = "switch window down" })
map({"n", "t", "v", "i"}, "<C-k>", "<C-w>k", { desc = "switch window up" })
map("n", "<leader>.", "<cmd>Telescope find_files<cr>", { desc = "telescope find files" })
map("n", "<leader>/", "<cmd>Telescope live_grep<cr>", { desc = "telescope live grep" })
map("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "Neogit status buffer" })
map("n", "<leader>c", "gcc", { desc = "Toggle Comment", remap = true })
map("v", "<leader>c", "gc", { desc = "Toggle comment", remap = true })
map("n", "<leader>pp", "<cmd>Telescope workspaces<cr>", { desc = "telescope workspaces" })
map("n", "<leader>pf", "<cmd>Telescope find_files<cr>", { desc = "telescope find files" })
map("n", "<A-w>", "<cmd>q<cr>", { desc = "quit" })
map("n", "<leader>wd>", "<cmd>q<cr>", { desc = "quit" })
map("n", "<leader>ws", "<cmd>split<cr>", { desc = "horizontal split" })
map("n", "<leader>wS", "<cmd>split<cr>", { desc = "horizontal split" })
map("n", "<leader>wv", "<cmd>vsplit<cr>", { desc = "vertical split" })
map("n", "<leader>wV", "<cmd>vsplit<cr>", { desc = "vertical split" })
map("n", "<A-p>", "<cmd>edit #<cr>", { desc = "edit previous" })
map({ "n", "t", "i", "v" }, "<A-f>", "<cmd>NvimTreeToggle<CR>", { desc = "nvimtree toggle window" })
map({ "n", "t", "i", "v" }, "<A-z>", function()
  require("nvchad.term").toggle { pos = "sp", id = "htoggleTerm" }
end, { desc = "terminal toggleable horizontal term" })
map("i", "jk", "<ESC>")

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
