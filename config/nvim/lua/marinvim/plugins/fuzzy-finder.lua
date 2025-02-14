return {
  {
    "ibhagwan/fzf-lua",
    priority = 1001,
    lazy = false,
    config = function()
      require('fzf-lua').setup({
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept"
          }
        },
        winopts = {
          backdrop = 100
        },
        defaults = {
          hidden = true
        }
      })

      local picker = require('fzf-lua')
      picker.register_ui_select()


      -- Finding
      vim.keymap.set('n', '<leader>f', picker.files, { desc = "Find Files" })
      vim.keymap.set('n', '<leader>bl', picker.buffers, { desc = "List Buffers" })
      vim.keymap.set('n', '<leader>bt', picker.treesitter, { desc = "Buffer Treesitter Symbols" })
      vim.keymap.set('n', '<leader>dl', picker.diagnostics_workspace, { desc = "List Diagnostics" })
      -- Utility
      vim.keymap.set('n', '<leader>um', function()
        picker.files({ cwd = vim.fn.stdpath('config') })
      end, { desc = "Find Config File" })

      vim.keymap.set('n', '<leader>ud', function()
        picker.files({ cwd = '~/.config' })
      end, { desc = "Find Dotfiles" })
      -- Grep
      vim.keymap.set('n', '<leader>/', picker.grep, { desc = "Grep on user input" })
      vim.keymap.set('n', '<leader>*', picker.grep_cword, { desc = "Grep word under cursor" })
      vim.keymap.set('n', '<leader><leader>/', picker.live_grep_native, { desc = "Live Grep" })
      --Git
      vim.keymap.set('n', '<leader><leader>f', picker.git_files, { desc = "Find Git Files" })
      vim.keymap.set('n', '<leader>gc', picker.git_commits, { desc = "Git Commits" })
      vim.keymap.set('n', '<leader>gC', picker.git_bcommits, { desc = "Git Buffer Commits" })
      vim.keymap.set('n', '<leader>gb', picker.git_branches, { desc = "Git Branches" })
      vim.keymap.set('n', '<leader>gt', picker.git_status, { desc = "Git Status" })
      vim.keymap.set('n', '<leader>gh', picker.git_stash, { desc = "Git Stash" })
      vim.keymap.set('n', '<leader>gl', picker.git_blame, { desc = "Git Blame Buffer" })
      -- Helpful
      vim.keymap.set('n', '<leader>?t', picker.helptags, { desc = "Show Help Tags" })
      vim.keymap.set('n', '<leader>?b', picker.builtin, { desc = "Show Builtins" })
      vim.keymap.set('n', '<leader>?k', picker.keymaps, { desc = "Show Keymaps" })
      vim.keymap.set('n', '<leader>?c', picker.commands, { desc = "Show Commands" })
      vim.keymap.set('n', '<leader>?h', picker.command_history, { desc = "Show Command History" })
      vim.keymap.set('n', '<leader>?m', picker.man_pages, { desc = "Show Man Pages" })
      vim.keymap.set('n', '<leader>?j', picker.jumps, { desc = "Show Jump Lists" })
      vim.keymap.set('n', '<leader>?l', picker.loclist, { desc = "Show Location Lists" })
      vim.keymap.set('n', '<leader>?s', picker.search_history, { desc = "Show Search History" })
      vim.keymap.set('n', '<leader>?r', picker.registers, { desc = "List Registers" })
      vim.keymap.set('n', '<leader>?M', picker.marks, { desc = "Show Marks" })
    end
  }
}
