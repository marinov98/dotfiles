return {
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      -- Setup
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          layout_strategy = "horizontal",
          layout_config = {
            horizontal = {
              prompt_position = "top",
            },
          },
          sorting_strategy = "ascending"
        },
        extensions = {
          ["ui-select"] = {
            require("telescope.themes").get_dropdown({})
          },
          fzf = {}
        },
        pickers = {
          buffers = {
            theme = 'ivy'
          },
          diagnostics = {
            theme = 'ivy'
          },
          live_grep = {
            additional_args = { "--hidden" }
          },
          grep_string = {
            additional_args = { "--hidden" }
          }
        }

      })

      -- Extensions
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")

      -- Keybindings
      local picker = require("telescope.builtin")

      -- Essentials
      local ff_command = { 'rg', '--files', "--hidden", "--color", "never" }

      -- priority for find_files should be fd if you ask me
      if vim.fn.executable('fd') == 1 then
        ff_command = { 'fd', '--type', 'f', "--color=never", '--hidden' }
      elseif vim.fn.executable('fdfind') == 1 then
        ff_command = { 'fdfind', '--type', 'f', "--color=never", '--hidden' }
      end

      vim.keymap.set('n', '<leader>f', function() picker.find_files({ find_command = ff_command }) end,
        { desc = "Fuzzy find files in current working directory" })

      vim.keymap.set('n', '<leader>um', function() picker.find_files({ cwd = vim.fn.stdpath('config') }) end,
        { desc = "Fuzzy Find in Nvim configuration" })

      vim.keymap.set('n', '<leader>/', function()
        picker.grep_string({ search = vim.fn.input("Grep > ") })
      end, { desc = "Grep on user input" })
      vim.keymap.set('n', '<leader>*', picker.grep_string, { desc = "Grep word under cursor" })
      vim.keymap.set('n', '<leader>bl', picker.buffers, { desc = "List buffers" })
      vim.keymap.set('n', '<leader>dl', picker.diagnostics, { desc = "List diagnostics" })
      -- Project Specific
      vim.keymap.set('n', '<leader><leader>/', picker.live_grep, { desc = "Live Grep" })
      vim.keymap.set('n', '<leader><leader>f', picker.git_files, { desc = "Find git files" })
      -- Git specific
      vim.keymap.set('n', '<leader>gc', picker.git_commits, { desc = "List git commits" })
      vim.keymap.set('n', '<leader>gC', picker.git_bcommits, { desc = "List git bcommits" })
      vim.keymap.set('n', '<leader>gb', picker.git_branches, { desc = "List git branches" })
      vim.keymap.set('n', '<leader>gt', picker.git_status, { desc = "Show git status" })
      vim.keymap.set('n', '<leader>gh', picker.git_stash, { desc = "Git stash" })
      -- Helpful
      vim.keymap.set('n', '<leader>?t', picker.help_tags, { desc = "Show help tags" })
      vim.keymap.set('n', '<leader>?b', picker.builtin, { desc = "Show telescope builtins" })
      vim.keymap.set('n', '<leader>?k', picker.keymaps, { desc = "Show keymaps" })
      vim.keymap.set('n', '<leader>?c', picker.commands, { desc = "List commands" })
      vim.keymap.set('n', '<leader>?h', picker.command_history, { desc = "Show command history" })
      vim.keymap.set('n', '<leader>?m', picker.man_pages, { desc = "List man pages" })
      vim.keymap.set('n', '<leader>?j', picker.jumplist, { desc = "List jumplists" })
      vim.keymap.set('n', '<leader>?l', picker.loclist, { desc = "Show loclist" })
      vim.keymap.set('n', '<leader>?s', picker.search_history, { desc = "Show search history" })
      vim.keymap.set('n', '<leader>?r', picker.registers, { desc = "List registers" })
    end
  }
}
