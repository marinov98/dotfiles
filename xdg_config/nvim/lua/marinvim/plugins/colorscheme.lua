return {
  {
    "folke/tokyonight.nvim",
    opts = {
      style = "night",
    },
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    opts = {
      flavour = "mocha",
    },
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
  },
  {
    "EdenEast/nightfox.nvim",
  },
  {
    "scottmckendry/cyberdream.nvim",
    opts = { variant = "dark" },
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require("cyberdream").setup(opts)
      vim.cmd.colorscheme("cyberdream")
    end
  }
}
