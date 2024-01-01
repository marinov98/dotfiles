return {
  "nvim-lualine/lualine.nvim",
  config = function()
    require("lualine").setup({
      options = {
        theme = 'catppuccin',
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diagnostics' },
        lualine_c = { 'filename' },
        lualine_x = { 'encoding', 'filetype' },
        lualine_y = {},
        lualine_z = {}
      },
    })
  end
}
