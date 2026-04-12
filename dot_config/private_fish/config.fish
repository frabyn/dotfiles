if status is-interactive
    # Tool integrations
    type -q direnv; and direnv hook fish | source
    type -q fzf; and fzf --fish | source
    type -q zoxide; and zoxide init fish | source
    type -q uv; and uv generate-shell-completion fish | source
    type -q tailscale; and tailscale completion fish | source
    type -q fnm; and fnm env --use-on-cd --shell fish | source
end
