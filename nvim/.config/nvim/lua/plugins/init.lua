return {
  -- Theme
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
  -- Productivity
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
          },
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          }
        }
      }
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
      vim.keymap.set("n", "<leader>fb", ":Telescope file_browser<CR>")
    end
  },
  {
    "nvim-telescope/telescope-file-browser.nvim", dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {},
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
  },
  -- Programming
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
    config = true,
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = {},
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "neovim/nvim-lspconfig",
    },
    config = function()
      require("mason").setup()
      require("mason-lspconfig").setup()

      vim.keymap.set('n', '<space>d', vim.diagnostic.open_float)
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
    end
  },
  { "hrsh7th/cmp-nvim-lsp" },
  {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    -- install jsregexp (optional!).
    build = "make install_jsregexp"
  },
  {
    "hrsh7th/nvim-cmp",
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        sources = {
          { name = "nvim_lsp" }
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        snippet = {
          expand = function(args)
            require('luasnip').lsp_expand(args.body)
          end,
        }
      })
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      vim.keymap.set('n', ']g', ':Gitsigns next_hunk<CR>')
      vim.keymap.set('n', '[g', ':Gitsigns prev_hunk<CR>')
      vim.keymap.set('n', '<leader>gp', ':Gitsigns preview_hunk<CR>')
      vim.keymap.set('n', '<leader>gd', ':Gitsigns diffthis<CR>')
    end
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {}
  }
}

