# Toolbox container detection
if test -f /run/.toolboxenv
    set -l name (string match -r 'name="([^"]+)"' < /run/.containerenv)[2]
    if test -n "$name" -a -f $HOME/.config/containers/toolbox-$name.rc
        source $HOME/.config/containers/toolbox-$name.rc
    end
end
