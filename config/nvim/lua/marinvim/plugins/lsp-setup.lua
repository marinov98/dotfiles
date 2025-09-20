return {
  {
    "williamboman/mason.nvim",
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall' },
    config = function()
      require("mason").setup()
    end
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls" } -- examples: "pyright", "cssls", "ts_ls", "jsonls"
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = { "saghen/blink.cmp" },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local capabilities = require("blink.cmp").get_lsp_capabilities()
      local servers = {
        "pyright",
        "ts_ls",
        "jsonls",
        "yamlls",
        "html",
        "cssls",
        "gopls",
        "rust_analyzer",
        "elixirls",
      }

      for _, server in ipairs(servers) do
        vim.lsp.config[server] = vim.tbl_deep_extend("force", vim.lsp.config[server] or {}, {
          capabilities = capabilities,
        })
      end

      -- Special config for lua_ls
      vim.lsp.config.lua_ls = vim.tbl_deep_extend("force", vim.lsp.config.lua_ls or {}, {
        capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim", "it", "describe", "Snacks", "MiniStatusline" },
            },
          },
        },
      })

      -- Finally enable them all
      vim.lsp.enable(vim.list_extend({ "lua_ls" }, servers))

      -- Keymaps / autocmd on attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(ev)
          vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
          local local_buf = ev.buf

          -- Diagnostics navigation
          vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Prev diagnostic", buffer = local_buf })
          vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic", buffer = local_buf })
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
        end,
      })
    end,
  }
}
