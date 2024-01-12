return
{
  {
    "NvChad/nvim-colorizer.lua",
    version = "*",
    event = { 'BufReadPre', 'BufNewFile' },
    config = true
  },
  {
    "windwp/nvim-ts-autotag",
    dependencies = "nvim-treesitter/nvim-treesitter",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function ()
      require("nvim-ts-autotag").setup()
    end,
  }
}
