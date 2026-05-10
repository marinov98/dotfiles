return {
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      {
        "mason-org/mason.nvim",
        cmd = { 'Mason', 'MasonInstall', 'MasonUninstall' },
        opts = {}
      },
      "neovim/nvim-lspconfig",
    },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        require("blink.cmp").get_lsp_capabilities())

      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls",
          "ty",
          "ruff",
          "ts_ls",
          "rust_analyzer",
        }
      })

      vim.lsp.config("*", {
        capabilities = capabilities
      })

      vim.lsp.config("lua_ls", {
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
          },
        },
      })


      local severity = vim.diagnostic.severity
      vim.diagnostic.config({
        signs = {
          text = {
            [severity.ERROR] = "",
            [severity.WARN]  = "",
            [severity.INFO]  = "",
            [severity.HINT]  = "",
          },
        },
        virtual_text = true,
        float = {
          focusable = true,
          border = "rounded",
        }
      })

      -- Keymaps / autocmd on attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = require("marinvim.lsp").map_on_attach
      })
    end,
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
            if require("conform").get_formatter_info("ruff_format", bufnr).available then
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
