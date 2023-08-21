---
Author: Lucas Elvira Martín
date: 2023-04-03
alias: Bash Scrips
tag: CS/TOOLS/LANGUAGE
---
area:: [[../04_INDEX_NOTES/PROGRAMMING_LANGUAGES|programming languages]]

# Bash Scripts

This folder contains a list of custom script. To make them work, you need to add
the folder to your path. For example, if you store them in ~/.local/bin/, you
can add this folder to path adding it to your profile file

Also remember change the permission of the file to 766 to enable execute them

## Structure

On this section, I will describe how the folder is structure and the porpoise of
each script
```bash
tree .
├── connect.sh
├── general
│   ├── https -> /home/lucas/Documents/git/https_server/index.js
│   ├── remove_empty_dir.bash
│   └── uc3mconnect
├── i3
│   ├── bluetooth.sh
│   ├── getip.sh
│   ├── sp
│   └── togle_bluetooth.sh
├── README.md
├── scripts
│   ├── bluetooth.sh
│   ├── colors.sh
│   ├── getip.sh
│   ├── launch_polybar
│   ├── polybar_bluetooth_speaker.sh
│   ├── polybar_custom_time.sh
│   ├── polybar_dunst_indicator.sh
│   ├── polybar_healthbar.sh
│   ├── polybar_net_indicator.sh
│   ├── polybar_net_speed.sh
│   └── tresorit_indicator.sh
└── vim
    └── plantuml
```

### i3wm

There are some scipts that I use a lot with i3wm

#### bluetooth.sh

This script allows to list the connected devices by bluetooth with the
bluetoothctl interface

#### togle_bluetooth.sh
Togle the connection with the bluetooth headphones. You need to know the mac
address of the devies

To call this, command I have  had configured a keybinding to super+F2

#### getip

This script is used by polybar to display the local ip or the ip given by the
VPN

#### sp

Script to control by command line spotify client. Also is used with polybar

### vim

Some scripts needed by a few plugings like plnatuml preview

### plantuml

execute the plantuml.jar for the current file and generate a svg image


### General

### remove_empty_dir

This script try to remove a directory which only contains empty folders

#### uc3mconnect

This script connect/disconnect with the vpn from UC3M. This script use nmcli as
interface. First you need to create the connection and then can easily connect
or disconnect from it 

#### https

Script written in nodejs to create a custom http/(s) server using auto-sign
certificate. This certificates should be in the same folder as the **realpath**
of the scrpt, inside another folder named `ssl` and the names of the key and
cert file should be `/ssl/key.pem` and `/ssl/cert.pem`

#### Projects

This script generate a interactive menu in bash to navigate from your currents
projects. Do it has a lot of thinks to learn. See [[TOOLS_BASH_SCRIPTS_PROJECTS|Projects documentation]]



<!-- vim: set spelllang=en: -->
