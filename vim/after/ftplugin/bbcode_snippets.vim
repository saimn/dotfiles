if !exists('loaded_snippet') || &cp
    finish
endif

exec "Snippet bb [b]<{}>[/b]"
exec "Snippet ii [i]<{}>[/i]"
exec "Snippet uu [u]<{}>[/u]"
exec "Snippet ss [s]<{}>[/s]"
exec "Snippet qq [q]<{}>[/q]"
exec "Snippet cc [c]<{}>[/c]"
exec "Snippet rig [right]<{}>[/right]"
exec "Snippet cen [center]<{}>[/center]"
exec "Snippet cod [code]<{}>[/code]"
exec "Snippet img [img]<{}>[/img]"
exec "Snippet url [url=<{url}>]<{name}>[/url]"
exec "Snippet col [color=<{color}>]<{text}>[/color]"
exec "Snippet quo [quote=<{author}>]<{text}>[/quote]"
