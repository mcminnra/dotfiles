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
}, {
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
}}
