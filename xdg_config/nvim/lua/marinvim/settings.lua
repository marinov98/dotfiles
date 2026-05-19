-- Netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3 -- Tree style view
-- Disable Netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Line Numbers
vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.mouse = 'a' -- enable the mouse in all modes
vim.opt.updatetime = 100
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.signcolumn = 'no'
vim.opt.laststatus = 3 -- global status
vim.opt.smartindent = true
vim.opt.wrap = false

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = false

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.pumheight = 10 -- how many candidates to show on a popup
vim.opt.completeopt = "menu,menuone,noselect"

vim.opt.termguicolors = true
vim.opt.splitbelow = true -- split go below
vim.opt.splitright = true -- vertical split to the right

-- clipboard, cursors
vim.opt.cursorline = true
vim.opt.clipboard = "unnamedplus" -- use system clipboard
vim.opt.errorbells = false        -- no sounds on error
