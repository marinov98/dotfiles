return {
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.5',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    config = function()
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>f', builtin.find_files, {})
      vim.keymap.set('n', 'gs', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>/', builtin.grep_string, {})
      vim.keymap.set('n', '<leader>ur', builtin.oldfiles, {})
      vim.keymap.set('n', '<leader>bi', builtin.buffers, {})
      vim.keymap.set('n', '<leader>dh', builtin.help_tags, {})
      vim.keymap.set('n', '<leader>ll', builtin.diagnostics, {})

      local telescope = require("telescope")
      telescope.load_extension("fzf")
    end
  },
  {
    "nvim-telescope/telescope-ui-select.nvim",
    config = function()
      require("telescope").setup({
        extensions = {
          ["ui-select"] = {
            require("telescope.themes").get_dropdown({})
          }
        }
      })
      -- To get ui-select loaded and working with telescope, you need to call
      -- load_extension, somewhere after setup function:
      require("telescope").load_extension("ui-select")
      vim.keymap.set('n', '<leader>?', function()
        require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
          winblend = 10,
          previewer = false
        }))
      end, {})
    end
  },
  {
    'nvim-telescope/telescope-project.nvim',
    config = function()
      require("telescope").load_extension("project")
      vim.keymap.set('n', '<leader><leader>', ":Telescope project<CR>", {})
    end
  }
}
