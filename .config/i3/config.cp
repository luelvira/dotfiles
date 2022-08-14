# vim: filetype=i3
# File originally by Luke Smith <https://lukesmith.xyz>
#
# Modified by machaerus
# https://gitlab.com/machaerus

# Colors
## class                 border  backgr. text    indicator child_border
#client.focused          #000000 #d79921 #ffffff #83a598   #458588
#client.focused_inactive $color0 #5f676a #ffffff #484e50   #A19A7F
#client.unfocused        $color0 #222222 #888888 #292d2e   $color0
#client.urgent           #2f343a #900000 #ffffff #900000   #900000
#client.placeholder      #000000 #0c0c0c #ffffff #000000   #0c0c0c
#client.background       #ffffff

# #---Colors to be used with WPGTK--# #
set $coloract "#cb2139"
set $colorin "#8b1727"
set_from_resource $color15 i3wm.color15 "#eaeadd"
set_from_resource $split i3wm.color10 "#cf8865"
set_from_resource $coloru i3wm.color2 "#946D5A"
set_from_resource $color0 i3wm.color0 "#121519"
set_from_resource $color8 i3wm.color8 "#29313c"

# class                 border    backgr    text    indicator child_border
client.focused          $coloract $coloract $color15 $split  $coloract
client.focused_inactive $color8   $coloract $color15 $split  $color8
#client.unfocused        $color0   $colorin  $color15 $split  $color0
client.urgent           $coloru   $coloru   $color15 $coloru $coloru
client.placeholder      $color0   $color8   $color15 $color0 $color8
client.background       $color0
#-------------------------------------------------------------#


# #---Basic Definitions---# #
for_window [class="^.*"] border pixel 3
gaps inner 10
gaps outer 0
set $term --no-startup-id i3-sensible-terminal
set $mod Mod4
set $shutdown sudo -A shutdown -h now
set $reboot sudo -A reboot
# set $hibernate sudo -A systemctl suspend
set $polybartoggle echo cmd:toggle > /tmp/ipc-polybar-bottom

# #---Dropdown Windows---# #
# General dropdown window traits. The order can matter.
for_window [instance="dropdown_*"] floating enable
for_window [instance="dropdown_*"] move scratchpad
for_window [instance="dropdown_*"] sticky enable
for_window [instance="dropdown_*"] scratchpad show
for_window [instance="dropdown_tmuxdd"] resize set 625 450
for_window [instance="dropdown_dropdowncalc"] resize set 800 300
for_window [instance="dropdown_tmuxdd"] border pixel 3
for_window [instance="dropdown_dropdowncalc"] border pixel 3
for_window [instance="dropdown_*"] move position center

focus_follows_mouse no

#exec_always --no-startup-id i3-cycle-focus.py --history 2
#bindsym $mod+Tab exec --no-startup-id i3-cycle-focus.py --switch
#bindsym $mod+Tab exec rofi -show window -show-icons
bindsym $mod+Tab exec alttab

# #---Basic Bindings---# #
bindsym $mod+Return             exec $term
bindsym $mod+Shift+Return       exec --no-startup-id samedir

bindsym $mod+Shift+space        floating toggle
bindsym $mod+space              focus mode_toggle

bindsym $mod+Escape             workspace prev

# bindsym $mod+BackSpace
bindsym $mod+Shift+BackSpace    exec --no-startup-id prompt "Reboot computer?" "$reboot"

bindsym $mod+d                  exec --no-startup-id rofi -show run
bindsym $mod+grave                      exec --no-startup-id rofi -show window

#STOP/HIDE EVERYTHING:
bindsym $mod+Shift+Delete       exec --no-startup-id amixer sset Master mute ; exec --no-startup-id mpc pause && pkill -RTMIN+10 i3blocks ; exec --no-startup-id pauseallmpv; workspace 0; exec $term -e htop ; exec $term -e $FILE

# Show selection:
bindsym Insert             exec --no-startup-id showclip

# #---Letter Key Bindings---# #
bindsym $mod+q                  [con_id="__focused__" instance="^(?!dropdown_).*$"] kill
#bindsym $mod+Shift+q           [con_id="__focused__" instance="^(?!dropdown_).*$"] exec --no-startup-id kill -9 `xdotool getwindowfocus getwindowpid`

bindsym $mod+Shift+w            exec --no-startup-id $BROWSER

bindsym $mod+e                  exec --no-startup-id emacsclient -c -a 'emacs'

# bindsym $mod+r                  exec $term -e $FILE
# bindsym $mod+Shift+r            exec --no-startup-id winresize
bindsym $mod+r                  exec --no-startup-id rofi -theme config_cmd -show run -modi run

bindsym $mod+t                  split toggle
bindsym $mod+Shift+r            gaps inner current set 15; gaps outer current set 15

bindsym $mod+y                  resize grow left
bindsym $mod+Shift+y            resize shrink left

bindsym $mod+u                  resize grow down
bindsym $mod+Shift+u            resize shrink down      

bindsym $mod+i                  resize grow up
bindsym $mod+Shift+i            resize shrink up        

bindsym $mod+o                  resize grow right
bindsym $mod+Shift+o            resize shrink right

bindsym $mod+p                  exec --no-startup-id mpc toggle
bindsym $mod+Shift+p            exec --no-startup-id mpc pause

bindsym $mod+g                  gaps inner current plus 5
bindsym $mod+Shift+g            gaps inner current minus 5

bindsym $mod+f                  fullscreen toggle
bindsym $mod+Shift+f            mode "$freeze" ;; exec --no-startup-id notify-send "Distraction-free mode activated." "Press Super+Shift+f to return."

# bindsym $mod+g                  workspace prev

bindsym $mod+h                  focus left
bindsym $mod+Shift+h            move left 30
bindsym $mod+Ctrl+h             move workspace to output left

bindsym $mod+j                  focus down
bindsym $mod+Shift+j            move down 30
bindsym $mod+Ctrl+j             move workspace to output down

bindsym $mod+k                  focus up
bindsym $mod+Shift+k            move up 30
bindsym $mod+Ctrl+k             move workspace to output up

bindsym $mod+l                  focus right
bindsym $mod+Shift+l            move right 30
bindsym $mod+Ctrl+l             move workspace to output right

#bindsym $mod+z                  gaps outer current plus 5
#bindsym $mod+Shift+z            gaps outer current minus 5

# bindsym $mod+x                  exec --no-startup-id mpc -h 127.0.0.1 -p 6601 pause; exec --no-startup-id pauseallmpv ; exec --no-startup-id betterlockscreen -l dim ; exec --no-startup-id xset dpms force off
bindsym $mod+x                  exec --no-startup-id i3lock
bindsym $mod+Shift+x            exec --no-startup-id prompt "Shutdown computer?" "$shutdown"

#bindsym $mod+b                 bar mode toggle
bindsym $mod+b                  exec --no-startup-id $BROWSER
bindsym $mod+Shift+b            floating toggle; sticky toggle; exec --no-startup-id hover left

bindsym $mod+n                  exec --no-startup-id nemo
bindsym $mod+Shift+n            floating toggle; sticky toggle; exec --no-startup-id hover right

# #---Workspace Bindings---# #
bindsym $mod+semicolon          workspace next
bindsym $mod+apostrophe         split horizontal ;; exec $term
bindsym $mod+slash              split vertical ;; exec $term

set $ws1 "1"
set $ws2 "2"
set $ws3 "3"
set $ws4 "4"
set $ws5 "5"
set $ws6 "6"
set $ws7 "7"
set $ws8 "8"
set $ws9 "9"
set $ws10 "10"

# switch to workspace
bindsym $mod+1          workspace $ws1
bindsym $mod+2          workspace $ws2
bindsym $mod+3          workspace $ws3
bindsym $mod+4          workspace $ws4
bindsym $mod+5          workspace $ws5
bindsym $mod+6          workspace $ws6
bindsym $mod+7          workspace $ws7
bindsym $mod+8          workspace $ws8
bindsym $mod+9          workspace $ws9
bindsym $mod+0          workspace $ws10

# move focused container to workspace
bindsym $mod+Shift+1    move container to workspace $ws1
bindsym $mod+Shift+2    move container to workspace $ws2
bindsym $mod+Shift+3    move container to workspace $ws3
bindsym $mod+Shift+4    move container to workspace $ws4
bindsym $mod+Shift+5    move container to workspace $ws5
bindsym $mod+Shift+6    move container to workspace $ws6
bindsym $mod+Shift+7    move container to workspace $ws7
bindsym $mod+Shift+8    move container to workspace $ws8
bindsym $mod+Shift+9    move container to workspace $ws9
bindsym $mod+Shift+0    move container to workspace $ws10

for_window [window_role="GtkFileChooserDialog"] resize set 800 600
for_window [window_role="GtkFileChooserDialog"] move position center

for_window [class="Firefox"] move workspace $ws2
for_window [class="Slack"] move workspace $ws4
for_window [class="Nautilus"] move workspace $ws5
for_window [class="virtualbox"] move workspace $ws10

# Bindings to make the webcam float and stick.
for_window [title="mpvfloat"] floating enable
for_window [title="mpvfloat"] sticky enable
for_window [title="mpvfloat"] border pixel 0
no_focus [title="mpvfloat"]

for_window [title="calcurse"] floating enable
# for_window [title="calcurse"] resize set 1200 800

# #---Function Buttons---# #
bindsym $mod+v          exec --no-startup-id $polybartoggle
bindsym $mod+F1         restart
bindsym $mod+F2         exec --no-startup-id launch_polybar
bindsym $mod+F8         exec --no-startup-id rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}'
bindsym $mod+F12		exec $term -e calcurse

# #---Arrow Keys---# #
bindsym $mod+Left               focus left
bindsym $mod+Shift+Left         move left
bindsym $mod+Ctrl+Left          move workspace to output left
bindsym $mod+Down               focus down
bindsym $mod+Shift+Down         move down
bindsym $mod+Ctrl+Down          move workspace to output down
bindsym $mod+Up                 focus up
bindsym $mod+Shift+Up           move up
bindsym $mod+Ctrl+Up            move workspace to output up
bindsym $mod+Right              focus right
bindsym $mod+Shift+Right        move right
bindsym $mod+Ctrl+Right         move workspace to output right

# #---Media Keys---# #
# Volume keys
bindsym $mod+plus               exec --no-startup-id amixer sset Master 5%+ && pkill -RTMIN+10 i3blocks
bindsym $mod+Shift+plus         exec --no-startup-id amixer sset Master 15%+ && pkill -RTMIN+10 i3blocks
bindsym $mod+minus              exec --no-startup-id amixer sset Master 5%- && pkill -RTMIN+10 i3blocks
bindsym $mod+Shift+minus        exec --no-startup-id amixer sset Master 15%- && pkill -RTMIN+10 i3blocks
bindsym $mod+less               exec --no-startup-id mpc prev
bindsym $mod+Shift+less         exec --no-startup-id mpc seek 0%
bindsym $mod+greater            exec --no-startup-id mpc next
bindsym $mod+Shift+greater      exec --no-startup-id mpc next

# For advancing forward/backward in an mpd song
bindsym $mod+bracketleft        exec --no-startup-id mpc seek -10
bindsym $mod+Shift+bracketleft  exec --no-startup-id mpc seek -120
bindsym $mod+bracketright       exec --no-startup-id mpc seek +10
bindsym $mod+Shift+bracketright exec --no-startup-id mpc seek +120

# For screenshots and recording
bindsym Print                   exec --no-startup-id maim pic-full-"$(date '+%y%m%d-%H%M-%S').png" && notify-send "Screenshot captured!"
bindsym Ctrl+Print              exec --no-startup-id flameshot gui
bindsym $mod+Scroll_Lock        exec --no-startup-id "killall screenkey || screenkey"

# #---Extra XF86 Keys---# #
bindsym XF86AudioMute          exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && amixer sset Master toggle

bindsym XF86AudioLowerVolume    exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -4000
bindsym XF86AudioRaiseVolume    exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +4000

bindsym XF86PowerOff            exec --no-startup-id prompt "Shutdown computer?" "$shutdown"
bindsym XF86AudioMicMute       exec $micmute
bindsym XF86MonBrightnessDown exec "brightnessctl s 100-; notify-send 'brightness down'"
bindsym XF86MonBrightnessUp exec "brightnessctl s +100; notify-send 'brightness up'"
bindsym XF86Display             exec --no-startup-id displayselect

bindsym $mod+Shift+t layout tabbed
bindsym $mod+Shift+s layout stacked
bindsym $mod+Shift+e layout toggle split
