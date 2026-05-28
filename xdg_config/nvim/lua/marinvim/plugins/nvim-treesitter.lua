return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    event = { 'BufReadPre', 'BufNewFile' },
    init = function()
      -- Enable highlighting via FileType autocmd
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("MPMTreesitter", { clear = true }),
        callback = function(args)
          local buf = args.buf

          local ft = vim.bo[buf].filetype
          local lang = vim.treesitter.language.get_lang(ft) or ft
          local has_parser = pcall(vim.treesitter.get_parser, buf, lang)

          if has_parser then
            pcall(vim.treesitter.start, buf)
          end
        end,
      })
    end,
    config = function()
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
      local treesitter = require("nvim-treesitter")
      local alreadyInstalled = treesitter.get_installed()
      local parsersToInstall = vim.iter(ensureInstalled)
          :filter(function(parser)
            return not vim.tbl_contains(alreadyInstalled, parser)
          end)
          :totable()
      treesitter.install(parsersToInstall)
    end
  }
}
