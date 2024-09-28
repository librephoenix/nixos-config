vim.g.base46_cache = vim.fn.stdpath "data" .. "/nvchad/base46/"
vim.g.mapleader = " "

if vim.g.neovide then
  -- Helper function for transparency formatting
  local alpha = function()
      return string.format("%x", math.floor(255 * vim.g.transparency or 0.8))
  end
  vim.g.transparency = 0
  vim.g.neovide_background_color = vim.g.neovide_background_color .. alpha()
  vim.g.neovide_transparency = 0.8
  vim.g.neovide_floating_blur_amount_x = 8.0
  vim.g.neovide_floating_blur_amount_y = 8.0
  vim.g.neovide_refresh_rate = 120
  vim.g.neovide_cursor_vfx_mode = "pixiedust"
  vim.g.neovide_text_gamma = 0.8
  vim.g.neovide_text_contrast = 0.1
  vim.opt.termguicolors = true
  vim.g.neovide_scale_factor = 1.0
end

vim.o.conceallevel = 2

vim.api.nvim_create_user_command('W', 'execute "silent! write !sudo tee % >/dev/null" <bar> edit', { nargs = 0})


-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
  },

  { import = "plugins" },
}, lazy_config)

require("workspaces").setup({
    hooks = {
        open = { "Telescope find_files" },
    }
})

-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
require('telescope').setup {
  defaults = {
    winblend = 80,
  },
  pickers = {
    find_files = {
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
    workspaces = {
      -- keep insert mode after selection in the picker, default is false
      keep_insert = true,
      -- Highlight group used for the path in the picker, default is "String"
      path_hl = "String"
    }
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
require('telescope').load_extension('project')
require('telescope').load_extension('workspaces')

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "options"
require "nvchad.autocmds"

vim.schedule(function()
  require "mappings"
end)
