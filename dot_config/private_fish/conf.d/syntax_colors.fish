# Monokai syntax highlighting for fish
set -g fish_color_command A6E22E         # green — valid commands
set -g fish_color_keyword F92672         # pink — keywords (if, for, etc.)
set -g fish_color_quote E6DB74           # yellow — quoted strings
set -g fish_color_redirection 66D9EF     # cyan — redirections (>, |)
set -g fish_color_end F92672             # pink — statement terminators (;, &&)
set -g fish_color_error F92672 --bold    # pink bold — errors
set -g fish_color_param FD971F           # orange — parameters/arguments
set -g fish_color_option AE81FF          # purple — command options (--flag)
set -g fish_color_comment 75715E         # grey — comments
set -g fish_color_autosuggestion 75715E  # grey — autosuggestions
set -g fish_color_operator F92672        # pink — operators (*, ?)
set -g fish_color_escape AE81FF          # purple — escape sequences (\n, \t)
set -g fish_color_valid_path --underline # underline existing paths
set -g fish_color_search_match --background=49483E # monokai selection bg
set -g fish_color_selection --background=49483E
set -g fish_pager_color_prefix 66D9EF --underline  # cyan — completion prefix
set -g fish_pager_color_completion F8F8F2           # cream — completion text
set -g fish_pager_color_description 75715E          # grey — completion description
set -g fish_pager_color_selected_background --background=49483E
