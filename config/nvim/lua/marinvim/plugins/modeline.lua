return {
  "nvim-lualine/lualine.nvim",
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
  end
}
