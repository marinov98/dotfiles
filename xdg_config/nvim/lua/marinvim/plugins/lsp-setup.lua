return {
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "neovim/nvim-lspconfig",
    },
    opts = {
      ensure_installed = {
        "lua_ls",
        "ty",
        "ruff",
        "ts_ls"
      },
    }
  },
  {
    "mfussenegger/nvim-lint",
    keys = '<leader>cl',
    config = function()
      local lint = require("lint")

      lint.linters_by_ft = {
        javascript = { "biomejs" },
        typescript = { "biomejs" },
        javascriptreact = { "biomejs" },
        typescriptreact = { "biomejs" },
        python = { "ruff" },
      }
      vim.keymap.set("n", "<leader>cl", function()
        lint.try_lint()
      end, { desc = "Trigger linting for current file" })
    end
  },
  {
    "stevearc/conform.nvim",
    keys = { { '<leader>lf', mode = { 'n', 'v' } } },
    config = function()
      local conform = require("conform")

      conform.setup({
        formatters_by_ft = {
          python = function(bufnr)
            if conform.get_formatter_info("ruff_format", bufnr).available then
              return { "ruff_format", "ruff_organize_imports" }
            else
              return { "black", "isort" }
            end
          end,
          lua = { "stylua" },
          svelte = { "prettierd", "prettier", stop_after_first = true },
          javascript = { "biome", "biome-organize-imports" },
          typescript = { "biome", "biome-organize-imports" },
          javascriptreact = { "biome", "biome-organize-imports" },
          typescriptreact = { "biome", "biome-organize-imports" },
          json = { "biome", "biome-organize-imports", lsp_format = "fallback" },
          graphql = { "biome", "biome-organize-imports" },
          java = { "google-java-format" },
          markdown = { "prettierd", "prettier", stop_after_first = true },
          html = { "biome", "biome-organize-imports" },
          css = { "biome", "biome-organize-imports" },
          scss = { "biome", "biome-organize-imports" },
          rust = { "rustfmt", lsp_format = "fallback" },
        },
      })
      vim.keymap.set({ "n", "v" }, "<leader>lf", function()
        conform.format({
          lsp_fallback = true,
          async = true,
          timeout_ms = 800,
        })
      end, { desc = "Format file or range (in visual mode)" })
    end,
  }
}
