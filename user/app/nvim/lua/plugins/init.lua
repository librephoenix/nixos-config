return {
  {
    "stevearc/conform.nvim",
    -- event = 'BufWritePre', -- uncomment for format on save
    opts = require "configs.conform",
  },

  -- These are some examples, uncomment them if you want to see them work!
  {
    "neovim/nvim-lspconfig",
    config = function()
      require "configs.lspconfig"
    end,
  },

  {
  	"nvim-treesitter/nvim-treesitter",
  	opts = {
  		ensure_installed = {
  			"vim", "lua", "vimdoc",
       "html", "css", 'gdscript'
  		},
  	},
  },

  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make', lazy = false, },
  { 'nvim-telescope/telescope-project.nvim', lazy = false, },
  { 'natecraddock/workspaces.nvim', lazy = false, },
  { 'jghauser/follow-md-links.nvim', lazy = false, },

  {
    "NeogitOrg/neogit",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "sindrets/diffview.nvim",        -- optional - Diff integration
  
      -- Only one of these is needed.
      "nvim-telescope/telescope.nvim", -- optional
    },
    config = true
  },
  {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
      -- Setup orgmode
      require('orgmode').setup({})
    end,
  },
}
