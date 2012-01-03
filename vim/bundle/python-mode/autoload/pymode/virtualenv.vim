fun! pymode#virtualenv#Activate() "{{{

    for env in g:pymode_virtualenv_enabled
        if env == $VIRTUAL_ENV
            return 0
        endif
    endfor

    call add(g:pymode_virtualenv_enabled, $VIRTUAL_ENV)
    echomsg "Enabled virtualenv: " . $VIRTUAL_ENV

python << EOF
ve_dir = os.environ['VIRTUAL_ENV']
ve_dir in sys.path or sys.path.insert(0, ve_dir)
activate_this = os.path.join(os.path.join(ve_dir, 'bin'), 'activate_this.py')
execfile(activate_this, dict(__file__=activate_this))
EOF
endfunction "}}}
