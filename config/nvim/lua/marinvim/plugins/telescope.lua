return {
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.5',
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
          }
        },
        pickers = {
          live_grep = {
            additional_args = function()
              return { '--hidden', '--glob', '!**/.git/*' }
            end
          },
          grep_string = {
            additional_args = function()
              return { '--hidden', '--glob', '!**/.git/*' }
            end
          }
        }

      })

      -- Extensions
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")

      -- Keybindings
      local builtin = require("telescope.builtin")
      -- Essentials
      if vim.fn.executable('fd') == 1 then
        vim.keymap.set('n', '<leader>f', function()
          builtin.find_files({ find_command = { 'fd', '--type', 'f', "--color=never", '--hidden', '--exclude', ".git" } })
        end, {})
      elseif vim.fn.executable('rg') == 1 then
        vim.keymap.set('n', '<leader>f', function()
          builtin.find_files({ find_command = { 'rg', '--files', "--hidden", "--color", "never", "--glob=!**/.git/*" } })
        end, {})
      else -- I cannot save you if it gets to here
        vim.keymap.set('n', '<leader>f', builtin.find_files, {})
      end
      vim.keymap.set('n', '<leader>/', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>*', builtin.grep_string, {})
      vim.keymap.set('n', '<leader>ur', builtin.oldfiles, {})
      vim.keymap.set('n', '<leader>bi', builtin.buffers, {})
      vim.keymap.set('n', '<leader>ld', builtin.diagnostics, {})
      -- Git specific
      vim.keymap.set('n', '<leader>gf', builtin.git_files, {})
      vim.keymap.set('n', '<leader>gc', builtin.git_commits, {})
      vim.keymap.set('n', '<leader>gC', builtin.git_bcommits, {})
      vim.keymap.set('n', '<leader>gb', builtin.git_branches, {})
      vim.keymap.set('n', '<leader>gt', builtin.git_status, {})
      vim.keymap.set('n', '<leader>gh', builtin.git_stash, {})
      -- Helpful
      vim.keymap.set('n', '<leader>?t', builtin.help_tags, {})
      vim.keymap.set('n', '<leader>?k', builtin.keymaps, {})
      vim.keymap.set('n', '<leader>?c', builtin.commands, {})
      vim.keymap.set('n', '<leader>?h', builtin.command_history, {})
      vim.keymap.set('n', '<leader>?m', builtin.man_pages, {})
      vim.keymap.set('n', '<leader>?j', builtin.jumplist, {})
      vim.keymap.set('n', '<leader>?l', builtin.loclist, {})
      vim.keymap.set('n', '<leader>?s', builtin.search_history, {})
      vim.keymap.set('n', '<leader>?r', builtin.registers, {})
    end
  }
}
