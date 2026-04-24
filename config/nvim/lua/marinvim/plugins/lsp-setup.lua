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


      vim.diagnostic.config({
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "",
            [vim.diagnostic.severity.WARN]  = "",
            [vim.diagnostic.severity.INFO]  = "",
            [vim.diagnostic.severity.HINT]  = "",
          },
        },
        virtual_text = true,
        float = {
          focusable = false,
          border = "rounded",
        }
      })

      -- Keymaps / autocmd on attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(args)
          vim.bo[args.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
          local local_buf = args.buf

          -- Diagnostics navigation
          vim.keymap.set("n", "[d", function() vim.diagnostic.jump({ count = -1, float = false }) end,
            { desc = "Prev diagnostic", buffer = local_buf })
          vim.keymap.set("n", "]d", function() vim.diagnostic.jump({ count = 1, float = false }) end,
            { desc = "Next diagnostic", buffer = local_buf })
          vim.keymap.set("n", "<leader>dg", vim.diagnostic.open_float, { desc = "Glance Diagnostic", buffer = local_buf })

          -- LSP utilities
          vim.keymap.set("n", "<leader>lg", vim.lsp.buf.hover, { desc = "LSP Hover", buffer = local_buf })
          vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, { desc = "Code Action", buffer = local_buf })
          vim.keymap.set("n", "<leader>lF", function()
            vim.lsp.buf.format({ async = true })
          end, { desc = "Format Document", buffer = local_buf })

          vim.keymap.set("n", "<leader>lwa", vim.lsp.buf.add_workspace_folder,
            { desc = "Add WS Folder", buffer = local_buf })
          vim.keymap.set("n", "<leader>lwr", vim.lsp.buf.remove_workspace_folder,
            { desc = "Remove WS Folder", buffer = local_buf })
          vim.keymap.set("n", "<leader>lh", function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = local_buf }), { bufnr = local_buf })
          end, { desc = "Toggle Inlay Hints", buffer = local_buf })

          vim.keymap.set("n", "grd", vim.lsp.buf.declaration, { desc = "Go to Declaration", buffer = local_buf })
          vim.keymap.set("n", "grn", vim.lsp.buf.rename, { desc = "Rename", buffer = local_buf })
          vim.keymap.set("n", "grh", vim.lsp.buf.signature_help, { desc = "Signature Help", buffer = local_buf })
          vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, { desc = "Signature Help", buffer = local_buf })

          -- Picker variant
          local picker = Snacks.picker
          vim.keymap.set("n", "gd", picker.lsp_definitions, { desc = "Definitions", buffer = local_buf })
          vim.keymap.set("n", "grr", picker.lsp_references, { desc = "References", buffer = local_buf })
          vim.keymap.set("n", "gri", picker.lsp_implementations, { desc = "Implementations", buffer = local_buf })
          vim.keymap.set("n", "gry", picker.lsp_type_definitions, { desc = "Type Definitions", buffer = local_buf })
          vim.keymap.set("n", "<leader>ls", picker.lsp_symbols, { desc = "Document Symbols", buffer = local_buf })
          vim.keymap.set("n", "<leader>lws", picker.lsp_workspace_symbols,
            { desc = "Workspace Symbols", buffer = local_buf })
          vim.keymap.set("n", "<leader>lci", picker.lsp_incoming_calls, { desc = "Incoming Calls", buffer = local_buf })
          vim.keymap.set("n", "<leader>lco", picker.lsp_outgoing_calls, { desc = "Outgoing Calls", buffer = local_buf })

          -- Document highlighting
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if client and client:supports_method("textDocument/documentHighlight") then
            local augroup = vim.api.nvim_create_augroup("LspDocumentHighlight", { clear = false })
            local timer = vim.uv.new_timer()
            vim.api.nvim_create_autocmd({ "CursorHold" }, {
              buffer = args.buf,
              group = augroup,
              callback = function()
                timer:start(300, 0, vim.schedule_wrap(vim.lsp.buf.document_highlight))
              end,
            })

            vim.api.nvim_create_autocmd("CursorMoved", {
              buffer = args.buf,
              group = augroup,
              callback = function()
                timer:stop()
                vim.lsp.buf.clear_references()
              end,
            })
          end
        end,
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
