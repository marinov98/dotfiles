return {
  "nvim-lualine/lualine.nvim",
  event = { 'BufReadPre', 'BufNewFile' },
  config = function()
    require("lualine").setup({
      options = {
        theme = 'catppuccin',
        component_separators = { left = "", right = "" },
        globalstatus = true,
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch' },
        lualine_c = { { 'filename', path = 1 }, 'diagnostics' },
        lualine_x = { 'encoding', 'fileformat', 'location' },
        lualine_y = {},
        lualine_z = {}
      },
    })
    vim.opt.showmode = false -- avoid duplicate showings of mode
    vim.opt.showcmd = false  -- don't show commands'
  end
}
