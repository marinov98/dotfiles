return {
  "nvim-lualine/lualine.nvim",
  event = { 'BufReadPre', 'BufNewFile' },
  config = function()
    require("lualine").setup({
      options = {
        theme = 'catppuccin',
        globalstatus = true,
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = {},
        lualine_c = { 'filename', 'diagnostics' },
        lualine_x = { 'encoding', "location" },
        lualine_y = {},
        lualine_z = {}
      },
    })
    vim.opt.showmode = false -- avoid duplicate showings of mode
    vim.cmd("set noshowcmd") -- don't show commands'
  end
}
