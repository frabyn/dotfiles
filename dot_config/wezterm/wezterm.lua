local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- Helper functions
local function is_found(str, pattern)
	return string.find(str, pattern) ~= nil
end

local function get_platform()
	local is_win = is_found(wezterm.target_triple, "windows")
	local is_linux = is_found(wezterm.target_triple, "linux")
	local is_mac = is_found(wezterm.target_triple, "apple")
	local os

	if is_win then
		os = "windows"
	elseif is_linux then
		os = "linux"
	elseif is_mac then
		os = "mac"
	else
		error("Unknown platform")
	end

	return {
		os = os,
		is_win = is_win,
		is_linux = is_linux,
		is_mac = is_mac,
	}
end

-- GPU configuration
local gpus = wezterm.gui.enumerate_gpus()
if #gpus > 0 then
	config.webgpu_preferred_adapter = gpus[1]
	config.front_end = "WebGpu"
end

-- Window configuration
config.window_padding = {
	left = 0,
	right = 0,
	top = 10,
	bottom = 7.5,
}
config.window_background_opacity = 0.85

local platform = get_platform()

config.color_scheme = "Monokai (terminal.sexy)"

config.line_height = 1.15

-- Tab bar configuration
config.enable_tab_bar = false

config.audible_bell = "Disabled"
config.visual_bell = {
	fade_in_duration_ms = 75,
	fade_out_duration_ms = 75,
	target = "CursorColor",
}

config.colors = {
	visual_bell = "#202020",
}

-- Cursor configuration
config.default_cursor_style = "SteadyUnderline"
-- config.cursor_blink_rate = 0

if platform.is_win then
	config.default_prog = { "pwsh", "-NoLogo" }
	config.launch_menu = {
		{ label = "PowerShell Core", args = { "pwsh", "-NoLogo" } },
		{ label = "PowerShell Desktop", args = { "powershell" } },
		{ label = "Command Prompt", args = { "cmd" } },
		{ label = "Nushell", args = { "nu" } },
		{
			label = "Git Bash",
			args = { "C:\\Users\\kevin\\scoop\\apps\\git\\current\\bin\\bash.exe" },
		},
	}
elseif platform.is_mac then
	config.default_prog = { "zsh", "-l" }
	config.launch_menu = {
		{ label = "Bash", args = { "bash", "-l" } },
		{ label = "Fish", args = { "/opt/homebrew/bin/fish", "-l" } },
		-- { label = 'Nushell', args = { '/opt/homebrew/bin/nu', '-l' } },
		{ label = "Zsh", args = { "zsh", "-l" } },
	}
  -- local mac_font = wezterm.font("SF Mono Powerline", { weight = "Light" })
	local mac_font = wezterm.font("Berkeley Mono", { weight = "Regular" })
	config.font = mac_font
	config.font_size = 20
	config.initial_rows = 40
	config.initial_cols = 110
elseif platform.is_linux then
	config.default_prog = { "zsh", "-l" }
	config.launch_menu = {
		{ label = "Bash", args = { "bash", "-l" } },
		{ label = "Fish", args = { "fish", "-l" } },
		{ label = "Zsh", args = { "zsh", "-l" } },
	}
	local linux_font = wezterm.font("BerkeleyMono Nerd Font")
	config.font_size = 16
	config.font = linux_font
	config.window_decorations = "RESIZE"
end

return config
