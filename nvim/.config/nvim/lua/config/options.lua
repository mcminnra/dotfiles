-- ~/.config/nvim/lua/config/options.lua

local opt = vim.opt

-- General
opt.mouse = "a"                    -- enable mouse support
opt.clipboard = "unnamedplus"      -- use system clipboard
opt.swapfile = false              -- disable swap files
opt.completeopt = "menu,menuone,noselect"

-- UI
opt.number = true                 -- show line numbers
opt.relativenumber = true         -- show relative line numbers
opt.cursorline = true            -- highlight current line
opt.splitbelow = true            -- horizontal splits go below
opt.splitright = true            -- vertical splits go right
opt.termguicolors = true         -- enable 24-bit RGB colors
opt.showmode = false             -- don't show mode (status line will)
opt.signcolumn = "yes"           -- always show sign column
opt.wrap = false                 -- don't wrap lines
opt.scrolloff = 8                -- keep 8 lines above/below cursor
opt.sidescrolloff = 8            -- keep 8 columns left/right of cursor

-- Search
opt.ignorecase = true            -- case insensitive search
opt.smartcase = true             -- case sensitive if uppercase present
opt.hlsearch = false             -- don't highlight search results
opt.incsearch = true             -- show search results as you type

-- Indentation
opt.expandtab = true             -- use spaces instead of tabs
opt.shiftwidth = 2               -- shift 2 spaces when tab
opt.tabstop = 2                  -- 1 tab == 2 spaces
opt.autoindent = true            -- auto indent new lines
opt.smartindent = true           -- smart indent

-- Performance
opt.updatetime = 250             -- faster completion
opt.timeoutlen = 300             -- time to wait for mapped sequence

-- Undo/Backup
opt.undofile = true              -- enable persistent undo
opt.backup = false               -- don't create backup files
opt.writebackup = false          -- don't backup before overwriting

-- Folding (if you use nvim-ufo or similar)
opt.foldcolumn = "1"
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldenable = true