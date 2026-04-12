# Editor & Pager configuration
if type -q nvim
    set -gx EDITOR nvim
else
    set -gx EDITOR vim
end
set -gx VISUAL $EDITOR

if type -q bat
    set -gx PAGER bat
else
    set -gx PAGER less
end
