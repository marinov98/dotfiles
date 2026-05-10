return {
  {
    'milanglacier/minuet-ai.nvim',
    enabled = false,
    dependencies = {
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require('minuet').setup {
        provider = 'openai_fim_compatible',
        n_completions = 1,
        context_window = 2048,
        provider_options = {
          openai_fim_compatible = {
            api_key = 'TERM',
            name = 'Ollama',
            end_point = 'http://localhost:11434/v1/completions',
            model = 'qwen2.5-coder:3b',
            optional = {
              max_tokens = 516,
              top_p = 0.9,
            },
          },
        },
        virtualtext = {
          auto_trigger_ft = { "python", "lua" },
          keymap = {
            accept = "<C-;>f",
            accept_line = '<C-;>l',
            prev = '<C-;>n',
            next = '<C-;>p',
            dismiss = '<C-;>d'
          },
        },
      }
    end,
  }
}
