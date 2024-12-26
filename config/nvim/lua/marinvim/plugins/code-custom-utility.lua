return {
  {
    'echasnovski/mini.pairs',
    version = "*",
    event = "InsertEnter",
    opts = {}
  },
  -- icons
  {
    'echasnovski/mini.icons',
    version = false,
    opts = {}
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
    },
    requires = {
      "echasnovski/mini.icons"
    },
    cmd = "Neotree",
    keys = '<leader>tn',
    config = function()
      require("neo-tree").setup({
        filesystem = {
          filtered_items = {
            visible = true,
            hide_dotfiles = false,
            hide_gitignored = false,
          },
        },
        default_component_configs = {
          icon = {
            provider = function(icon, node) -- setup a custom icon provider
              local text, hl
              local mini_icons = require("mini.icons")
              if node.type == "file" then          -- if it's a file, set the text/hl
                text, hl = mini_icons.get("file", node.name)
              elseif node.type == "directory" then -- get directory icons
                text, hl = mini_icons.get("directory", node.name)
                -- only set the icon text if it is not expanded
                if node:is_expanded() then
                  text = nil
                end
              end

              -- set the icon text/highlight only if it exists
              if text then
                icon.text = text
              end
              if hl then
                icon.highlight = hl
              end
            end,
          },
          kind_icon = {
            provider = function(icon, node)
              local mini_icons = require("mini.icons")
              icon.text, icon.highlight = mini_icons.get("lsp", node.extra.kind.name)
            end,
          },
        },
      })
      vim.keymap.set('n', '<leader>tn', ":Neotree toggle<CR>", {})
    end
  },
  {
    'stevearc/oil.nvim',
    version = "*",
    config = function()
      -- ensure netrw is disabled
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1

      require("oil").setup({
        view_options = {
          show_hidden = true
        }
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory with Oil" })
    end
  }
}
