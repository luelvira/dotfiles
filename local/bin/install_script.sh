#!/usr/bin/env sh


apt-get install wget curl build-essential git docker docker-compose make cmake \
    python3-virtualenv openvpn vim-nox xclip \
    -y

curl -LO https://github.com/obsidianmd/obsidian-releases/releases/download/v1.1.9/obsidian_1.1.9_amd64.deb

apt install ./obsidian_1.1.9_amd64.deb
rm obsidian_1.1.9_amd64.deb

