return {
  'mfussenegger/nvim-dap',
  dependencies = {
    'rcarriga/nvim-dap-ui',
  },
  cmd = { 'DapUIToggle', 'DapToggleRepl', 'DapToggleBreakpoint' },
  config = function()
    require("dapui").setup()
    -- Experimental need to add dap servers for this to work!
    local dap, dapui = require("dap"), require("dapui")

    dap.listeners.before.attach.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
      dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
      dapui.close()
    end

    vim.keymap.set("n", "<leader>dt", ":DapToggleBreakpoint<CR>")
    vim.keymap.set("n", "<leader>dc", ":DapContinue<CR>")
    vim.keymap.set("n", "<leader>dx", ":DapTerminate<CR>")
    vim.keymap.set("n", "<leader>do", ":DapStepOver<CR>")
  end,
}
