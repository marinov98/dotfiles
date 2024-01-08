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
      }
      vim.keymap.set("n", "<leader>cl", function()
        lint.try_lint()
      end, { desc = "Trigger linting for current file" })
    end
  },
  {
    "stevearc/conform.nvim",
    keys = { { '<leader>ls', mode = { 'n', 'v' } } },
    config = function()
      local conform = require("conform")

      conform.setup({
        formatters_by_ft = {
          python = { "black" },
          lua = { "stylua" },
          svelte = { { "prettierd", "prettier" } },
          javascript = { { "prettierd", "prettier" } },
          typescript = { { "prettierd", "prettier" } },
          javascriptreact = { { "prettierd", "prettier" } },
          typescriptreact = { { "prettierd", "prettier" } },
          json = { { "prettierd", "prettier" } },
          graphql = { { "prettierd", "prettier" } },
          java = { "google-java-format" },
          markdown = { { "prettierd", "prettier" } },
          html = { { "prettierd", "prettier" } },
          css = { { "prettierd", "prettier" } },
          scss = { { "prettierd", "prettier" } },
        },
      })
      vim.keymap.set({ "n", "v" }, "<leader>ls", function()
        conform.format({
          lsp_fallback = true,
          async = false,
          timeout_ms = 800,
        })
      end, { desc = "Format file or range (in visual mode)" })
    end,
  }
}
