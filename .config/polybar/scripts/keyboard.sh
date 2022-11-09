#!/usr/bin/env bash

case "$(xset -q | grep LED | awk '{print $10 }')" in \
	"00000000") KDB="EN" ;;
	"00000001") KDB="EN·" ;;
	"00000002") KDB="EN." ;;
	"00000003") KDB="EN˙" ;;
	"00001000") KDB="ES" ;;
	"00001001") KDB="ES·" ;;
	"00001002") KDB="ES." ;;
	"00001003") KDB="ES˙" ;;
	* ) KDB="unknown" ;;
 esac
 echo $KDB
