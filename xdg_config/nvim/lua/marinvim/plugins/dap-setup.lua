return {
  {
    'mfussenegger/nvim-dap',
    dependencies = {
      "nvim-neotest/nvim-nio",
      'rcarriga/nvim-dap-ui',
      "mfussenegger/nvim-dap-python",
    },
    cmd = { 'DapUIToggle', 'DapToggleRepl', 'DapToggleBreakpoint', 'DapContinue' },
    keys = { '<leader>db', '<leader>dB', '<F5>', '<F7>' },
    config = function()
      local dap, dapui = require("dap"), require("dapui")

      dapui.setup()
      require("dap-python").setup("uv")

      -- Automatically open/close DAP UI
      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close

      vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Debug: Toggle Breakpoint" })
      vim.keymap.set("n", "<leader>dB", dap.set_breakpoint, { desc = "Debug: Set Breakpoint" })

      vim.keymap.set("n", "<F5>", dap.continue, { desc = "Debug: Start/Continue" })
      vim.keymap.set("n", "<F1>", dap.step_into, { desc = "Debug: Step Into" })
      vim.keymap.set("n", "<F2>", dap.step_over, { desc = "Debug: Step Over" })
      vim.keymap.set("n", "<F3>", dap.step_out, { desc = "Debug: Step Out" })
      vim.keymap.set("n", "<F7>", dapui.toggle, { desc = "Debug: See last session" })

      dap.adapters.lldb = {
        type = "executable",
        command = os.getenv("HOME") .. "/.local/share/nvim/mason/bin/codelldb",
        name = "codelldb",
      }

      local default_lldb_config = {
        {
          name = "Launch",
          type = "lldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          args = function()
            return vim.split(vim.fn.input("(Optional) Args: ", "", "file"), " ", { trimempty = true })
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
        },
      }
      dap.configurations.c = default_lldb_config
      dap.configurations.cpp = default_lldb_config
      dap.configurations.asm = default_lldb_config
      dap.configurations.zig = default_lldb_config
      dap.configurations.rust = vim.tbl_deep_extend("force", default_lldb_config, {
        {
          name = "Launch (Rust)",
          program = function()
            return vim.fn.input("Cargo executable: ", vim.fn.getcwd() .. "/target/debug/", "file")
          end,
        }
      })
    end,
  },
}
