function fish_prompt
    set -l last_status $status

    # user@host — monokai comment grey, orange on SSH
    if set -q SSH_TTY
        set_color FD971F
    else
        set_color 75715E
    end
    printf '%s@%s ' $USER (prompt_hostname)

    # cwd — monokai cyan
    set_color 66D9EF
    printf '%s' (prompt_pwd --full-length-dirs 2)

    # git
    set_color normal
    printf '%s' (fish_vcs_prompt ' (%s)')

    # error indicator — monokai pink
    if test $last_status -ne 0
        set_color F92672
        printf ' [%d]' $last_status
    end

    # prompt char — monokai green
    set_color A6E22E
    printf '\n❯ '
    set_color normal
end
