return {
  {
    "mfussenegger/nvim-lint",
    keys = '<leader>cl',
    config = function()
      local lint = require("lint")

      lint.linters_by_ft = {
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        typescriptreact = { "eslint_d" },
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
          python = { "ruff", "black" },
          lua = { "stylua" },
          svelte = { "prettier", "prettierd" },
          javascript = { "prettier", "prettierd" },
          typescript = { "prettier", "prettierd" },
          javascriptreact = { "prettier", "prettierd" },
          typescriptreact = { "prettier", "prettierd" },
          json = { "prettier", "prettierd", lsp_format = "fallback" },
          graphql = { "prettier", "prettierd" },
          java = { "google-java-format" },
          markdown = { "prettier", "prettierd" },
          html = { "prettier", "prettierd" },
          css = { "prettier", "prettierd" },
          scss = { "prettier", "prettierd" },
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
