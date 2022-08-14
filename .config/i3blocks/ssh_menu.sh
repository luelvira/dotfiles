#!/usr/bin/env bash

case $BLOCK_BUTTON in
	1) i3-msg -q -- exec rofi -modi ssh -show ssh -theme sshtheme
esac
echo " "
