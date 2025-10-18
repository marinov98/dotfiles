return {
  {
    "catppuccin/nvim",
    lazy = false,
    enabled = false,
    name = "catppuccin",
    priority = 1000,
    config = function()
      require('catppuccin').setup({ flavour = "mocha" })
      vim.cmd.colorscheme("catppuccin")
    end
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup({ style = "night" })
      vim.cmd.colorscheme("tokyonight")
    end,
  }
}
