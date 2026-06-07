return
{
  {
    "NvChad/nvim-colorizer.lua",
    opts = {},
    event = { 'BufReadPre', 'BufNewFile' },
  },
  {
    "windwp/nvim-ts-autotag",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {},
    event = { 'BufReadPre', 'BufNewFile' },
  }
}
