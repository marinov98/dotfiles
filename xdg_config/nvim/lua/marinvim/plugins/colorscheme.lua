local theme = "cyberdream"
return {
  {
    "folke/tokyonight.nvim",
    opts = {
      style = "night",
    },
    enabled = theme == "tokyonight",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require(theme).setup(opts)
      vim.cmd.colorscheme(theme)
    end
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    opts = {
      flavour = "mocha",
    },
    enabled = theme == "catppuccin",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require(theme).setup(opts)
      vim.cmd.colorscheme(theme)
    end
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    opts = {},
    enabled = theme == "rose-pine",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require(theme).setup(opts)
      vim.cmd.colorscheme(theme)
    end
  },
  {
    "EdenEast/nightfox.nvim",
    opts = {},
    enabled = theme == "carbonfox" or
        theme == "duskfox" or
        theme == "nightfox" or
        theme == "terafox" or
        theme == "dayfox" or
        theme == "nordfox",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require("nightfox").setup(opts)
      vim.cmd.colorscheme(theme)
    end
  },
  {
    "scottmckendry/cyberdream.nvim",
    opts = {
      variant = "dark",
      cache = true
    },
    enabled = theme == "cyberdream",
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require(theme).setup(opts)
      vim.cmd.colorscheme(theme)
    end
  }
}
