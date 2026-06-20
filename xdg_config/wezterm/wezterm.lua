local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- ==========================================
-- Appearance & Aesthetics
-- ==========================================
config.color_scheme = 'Jellybeans'
config.font = wezterm.font('JetBrainsMono Nerd Font')
config.font_size = 16.0

-- Cursor Customization
config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_rate = 0 -- Disables cursor blinking entirely

config.colors = {
  foreground = "#ffffff",
  background = "#16181a",

  selection_fg = "#ffffff",
  selection_bg = "#3c4048",

  scrollbar_thumb = "#16181a",
  split = "#16181a",

  ansi = { "#16181a", "#ff6e5e", "#5eff6c", "#f1ff5e", "#5ea1ff", "#bd5eff", "#5ef1ff", "#ffffff" },
  brights = { "#3c4048", "#ff6e5e", "#5eff6c", "#f1ff5e", "#5ea1ff", "#bd5eff", "#5ef1ff", "#ffffff" },
  indexed = { [16] = "#ffbd5e", [17] = "#ff6e5e" },
  cursor_bg = '#FFAE42',
  cursor_fg = "#16181a",
  cursor_border = '#FFAE42',
}

config.window_decorations = 'RESIZE'
config.max_fps = 120

-- Window Padding & Opacity
config.window_background_opacity = 0.95
config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 5,
}

config.audible_bell = "Disabled"
config.leader = { key = ',', mods = 'CTRL', timeout_milliseconds = 2000 }

config.keys = {
  -- Layout & Window Management
  { key = 'z', mods = 'LEADER',       action = wezterm.action.TogglePaneZoomState },
  { key = 'f', mods = 'LEADER|SHIFT', action = wezterm.action.ToggleFullScreen },
  { key = 'c', mods = 'LEADER',       action = wezterm.action.SpawnTab 'CurrentPaneDomain' },

  -- Splitting (with current directory preservation)
  { key = '|', mods = 'LEADER',       action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  { key = '-', mods = 'LEADER',       action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } },

  -- Pane Movement (Directional navigation)
  { key = 'h', mods = 'LEADER',       action = wezterm.action.ActivatePaneDirection 'Left' },
  { key = 'j', mods = 'LEADER',       action = wezterm.action.ActivatePaneDirection 'Down' },
  { key = 'k', mods = 'LEADER',       action = wezterm.action.ActivatePaneDirection 'Up' },
  { key = 'l', mods = 'LEADER',       action = wezterm.action.ActivatePaneDirection 'Right' },

  -- Tab Switching
  { key = 'p', mods = 'LEADER',       action = wezterm.action.ActivateTabRelative(-1) },
  { key = 'n', mods = 'LEADER',       action = wezterm.action.ActivateTabRelative(1) },

  -- Closing / Quitting
  { key = 'x', mods = 'LEADER',       action = wezterm.action.CloseCurrentPane { confirm = false } },
  { key = 'k', mods = 'LEADER|SHIFT', action = wezterm.action.CloseCurrentTab { confirm = false } },
  { key = 'q', mods = 'LEADER|SHIFT', action = wezterm.action.QuitApplication },
}

-- Quick loop to generate your 'ctrl+,' followed by 1, 2, 3, 4 tab jumps
for i = 1, 4 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'LEADER',
    action = wezterm.action.ActivateTab(i - 1), -- Lua loops are 1-indexed, but WezTerm tabs are 0-indexed
  })
end

return config
