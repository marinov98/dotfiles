return {
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.5',
    dependencies = { 
      'nvim-lua/plenary.nvim',
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    config = function()
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>f', builtin.find_files, {})
      vim.keymap.set('n', 'gs', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>bi', builtin.buffers, {})
      vim.keymap.set('n', '<leader>dh', builtin.help_tags, {})

	    local telescope = require("telescope")
      telescope.load_extension("fzf")
    end
  },
}

