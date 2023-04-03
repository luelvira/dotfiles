source colors.sh

dunst_notifications() {
	dunst_enabled=$(dunstctl is-paused)
	if [ "$dunst_enabled" == "false" ]; then
		dunst_indicator="${faded_green}${RESET}"
	else
		dunst_indicator="${faded_red}${RESET}"
	fi
	echo $dunst_indicator
}
dunst_notifications

