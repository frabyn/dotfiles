# Yay! High voltage and arrows!
# Based off miloshadzic theme
#
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

PROMPT='%{$fg[blue]%}$( if [[ "$name" == "arch-toolbox-latest" ]]; then echo "󰣇"; elif [[ "$name" == ubuntu* ]]; then echo ""; fi )%{$reset_color%}%{$fg[cyan]%}%1~%{$reset_color%}%{$fg[red]%}🚲%{$reset_color%}$(git_prompt_info)%{$fg[cyan]%}=>>%{$reset_color%}'
