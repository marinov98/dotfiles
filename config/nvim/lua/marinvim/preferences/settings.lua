vim.opt.guicursor = ""

-- Ignore compiled files
vim.opt.wildignore = "__pycache__"
vim.opt.wildignore:append { "*.o", "*~", "*.pyc", "*pycache*" }
vim.opt.wildignore:append { "Cargo.lock", "Cargo.Bazel.lock" }

-- Line Numbers
vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.mouse = 'a'						-- enable the mouse in all modes
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = false

vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.pumheight = 8 -- how many candidates to show on a popup

vim.opt.termguicolors = true
vim.opt.splitbelow = true				-- split go below
vim.opt.splitright = true				-- vertical split to the right

vim.opt.updatetime = 100

-- netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3

-- clipboard, cursors
vim.cmd("set clipboard=unnamedplus") -- use system clipboard
vim.cmd("set cursorline") -- highlight current line
vim.cmd("set noerrorbells") -- no sounds 
