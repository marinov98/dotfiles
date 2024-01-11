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
          "go",
          "sql",
          "rust",
          -- config
          "kdl", -- for zellij
          "xml",
          "cmake",
          "yaml",
          "toml",
          -- Web Dev
          "json",
          "javascript",
          "typescript",
          "tsx",
          "html",
          "css",
          "scss"
        },
        highlight = { enable = true },
        indent = { enable = true },
      })
    end
  }
}
