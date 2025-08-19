return {
  {
    "tiagovla/tokyodark.nvim",
    opts = {
      transparent_background = true
    },
    config = function(_, opts)
      require("tokyodark").setup(opts) -- calling setup is optional
      vim.cmd [[colorscheme tokyodark]]
    end,
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {'nvim-tree/nvim-web-devicons'},
    config = function()
      require('lualine').setup {
        options = {
          icons_enabled = true,
          theme = 'auto'
        }
      }
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    branch = 'master',
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup {
        -- enable syntax highlighting
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false
        },
        -- enable indentation
        indent = {
          enable = true
        },
        -- enable autotagging (w/ nvim-ts-autotag plugin)
        autotag = {
          enable = true
        },
        -- ensure these language parsers are installed
        ensure_installed = {"json", "javascript", "typescript", "tsx", "yaml", "html", "css", "markdown",
          "markdown_inline", "svelte", "bash", "lua", "dockerfile", "gitignore"},
        -- auto install above language parsers
        auto_install = true
      }
    end
  },
  {
    "lukas-reineke/headlines.nvim",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = true, -- or `opts = {}`
  },
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require("telescope").setup {
        pickers = {
          find_files = {
            hidden = true
          }
        },
        extensions = {
          file_browser = {
            hidden = { file_browser = true, folder_browser = true },
          }
        }
      }
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
      vim.keymap.set("n", "<space>fb", ":Telescope file_browser<CR>")
    end
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps (which-key)",
      },
    },
  },
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = false,
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("nvim-tree").setup {}
      vim.keymap.set('n', '<leader>tt', ':NvimTreeToggle<CR>')
      vim.keymap.set('n', '<leader>tf', ':NvimTreeFocus<CR>')
      vim.keymap.set('n', '<leader>tc', ':NvimTreeClose<CR>')
      vim.keymap.set('n', '<leader>tl', ':NvimTreeFindFile<CR>')
    end,
  }
}
