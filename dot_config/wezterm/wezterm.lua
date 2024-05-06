local wezterm = require("wezterm")

function scheme_for_appearance(appearance)
	if appearance:find 'Dark' then
	  return 'midnight-in-mojave'
	else
	  return 'Mostly Bright (terminal.sexy)'
	end
  end
  
  wezterm.on('window-config-reloaded', function(window, pane)
	local overrides = window:get_config_overrides() or {}
	local appearance = window:get_appearance()
	local scheme = scheme_for_appearance(appearance)
	if overrides.color_scheme ~= scheme then
	  overrides.color_scheme = scheme
	  window:set_config_overrides(overrides)
	end
  end)
  

return {

	font = wezterm.font("Berkeley Mono"),
	font_size = 18.0,
	hide_tab_bar_if_only_one_tab = true,
	window_frame = {
		font = wezterm.font({ family = "Berkeley Mono", weight = "Bold" }),
		font_size = 14.0,
	},
	window_background_opacity = 0.9,
	initial_cols = 100,
	initial_rows = 36,
	audible_bell = "Disabled",
	visual_bell = {
		fade_in_duration_ms = 75,
		fade_out_duration_ms = 75,
		target = "CursorColor",
	},

	colors = {
		visual_bell = "#202020",
	},
}
