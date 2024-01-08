return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local config = require("nvim-treesitter.configs")
      config.setup({
        ensure_installed = {
          -- mandatory (keep these as recommended by documentation)
          "c",
          "lua",
          "vim",
          "vimdoc",
          "query",
          -- preference (personal choice starts here)
          "python",
          "cpp",
          "java",
          "elixir",
          "json",
          "javascript",
          "typescript",
          "kdl", -- for zellij
          "xml",
          "rust",
          "yaml",
          "toml",
          "html"
        },
        highlight = { enable = true },
        indent = { enable = true },
      })
    end
  }
}
