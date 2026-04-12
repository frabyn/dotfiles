function fish_right_prompt
    # command duration (show if >1s) — monokai comment grey
    if test $CMD_DURATION -gt 1000
        set_color 75715E
        set -l secs (math --scale=1 "$CMD_DURATION / 1000")
        printf '%ss' $secs
        set_color normal
    end
end
