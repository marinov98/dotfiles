return {
  "NvChad/nvim-colorizer.lua",
  version = "*",
  config = function()
    require('colorizer').setup {
      filetypes = {
        'css',
        'javascript',
        'typescript',
        'html',
        "scss"
      },
    }
  end,
}
