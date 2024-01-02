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
        lualine_b = { 'branch', 'diagnostics' },
        lualine_c = { 'filename' },
        lualine_x = { 'encoding', { 'fileformat', icons_enabled = false } },
        lualine_y = {},
        lualine_z = {}
      },
    })
  end
}
