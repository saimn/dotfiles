# debian upgrade
# if the first argument is void, proceed local upgrade
# else, send command via ssh
# assume user have sufficient permission for upgrade
# without interaction
#
# Note:
# i use sudo with the follow lines
#
# Cmnd_Alias      DEBIAN  = /usr/bin/apt-get, /usr/bin/dpkg
# asyd            ALL = PASSWD: ALL, NOPASSWD: DEBIAN
upgrade () {
    if [ -z $1 ] ; then
        sudo apt-get update
        sudo apt-get -u upgrade
    else
        ssh $1 sudo apt-get update
        # ask before the upgrade
        local dummy
        ssh $1 sudo apt-get --no-act upgrade
        echo -n "Process the upgrade ?"
        read -q dummy
        if [[ $dummy == "y" ]] ; then
            ssh $1 sudo apt-get -u upgrade --yes
        fi
    fi
}

compdef _hosts upgrade

# Completion for apache 2 Debian tools
apache2_comp () {
	typeset mods_available mods_enabled sites_available sites_enabled

	mods_available=(/etc/apache2/mods-available/*)
	sites_available=(/etc/apache2/sites-available/*)

	mods_enabled=(/etc/apache2/mods-enabled/*)
	sites_enabled=(/etc/apache2/sites-enabled/*)

	# unicity, do a basename, and remove extension
	mods_available=${(u)mods_available:t:r}
	mods_enabled=${(u)mods_enabled:t:r}

	sites_available=${(u)sites_available:t}
	sites_enabled=${(u)sites_enabled:t}

	compdef "_wanted site expl 'Apache 2 site' compadd $sites_available" a2ensite
	compdef "_wanted site expl 'Apache 2 site' compadd $sites_enabled" a2dissite

	compdef "_wanted mod expl 'Apache 2 module' compadd $mods_available" a2enmod
	compdef "_wanted mod expl 'Apache 2 module' compadd $mods_enabled" a2dismod
}

[ -d /etc/apache2/sites-enabled ] &&
[ -d /etc/apache2/mods-enabled ] &&
	apache2_comp

