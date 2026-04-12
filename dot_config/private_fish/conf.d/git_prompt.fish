# fish_git_prompt configuration
set -g __fish_git_prompt_show_informative_status true
set -g __fish_git_prompt_showdirtystate true
set -g __fish_git_prompt_showuntrackedfiles true
set -g __fish_git_prompt_showupstream informative

# Status characters
set -g __fish_git_prompt_char_stateseparator ' '
set -g __fish_git_prompt_char_dirtystate '~'
set -g __fish_git_prompt_char_untrackedfiles '?'
set -g __fish_git_prompt_char_stagedstate '+'
set -g __fish_git_prompt_char_cleanstate ''

# Monokai colors
set -g __fish_git_prompt_color_branch AE81FF      # purple — branch name
set -g __fish_git_prompt_color_dirtystate E6DB74   # yellow — modified files
set -g __fish_git_prompt_color_untrackedfiles F92672 # pink — untracked
set -g __fish_git_prompt_color_stagedstate A6E22E  # green — staged
set -g __fish_git_prompt_color_upstream 75715E     # grey — ahead/behind
