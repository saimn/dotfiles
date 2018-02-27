#!/bin/bash
# -*- coding: utf-8 -*-

set -ie

mkdir $HOME/tmp
pushd $HOME/tmp

wget https://raw.githubusercontent.com/wee-slack/wee-slack/master/wee_slack.py
diff wee_slack.py $HOME/.weechat/python/wee_slack.py
mv -i wee_slack.py $HOME/.weechat/python

popd
exit 0
