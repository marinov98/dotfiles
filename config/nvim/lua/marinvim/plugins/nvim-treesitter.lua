return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    event = { 'BufReadPre', 'BufNewFile' },
    init = function()
      -- Install missing parsers without reinstalling already-installed ones
      local ensureInstalled = {
        -- mandatory
        "c",
        "lua",
        "vim",
        "vimdoc",
        "query",
        -- preference
        "python",
        "cpp",
        "java",
        "elixir",
        "go",
        "sql",
        "rust",
        -- config
        "kdl",
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
        "scss",
      }
      local alreadyInstalled = require("nvim-treesitter.config").get_installed()
      local parsersToInstall = vim.iter(ensureInstalled)
        :filter(function(parser)
          return not vim.tbl_contains(alreadyInstalled, parser)
        end)
        :totable()
      require("nvim-treesitter").install(parsersToInstall)

      -- Enable highlighting and indentation via FileType autocmd
      vim.api.nvim_create_autocmd("FileType", {
        callback = function()
          pcall(vim.treesitter.start)
          vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end,
      })
    end,
  }
}
