#!/usr/bin/env python2

# pypres -- Utility for enabling external displays based on presets
#           Inteded to be used with the "Presentation" media button
#           found on many laptops.
# Copyright (C) 2009 Adrian C. <anrxc_sysphere_org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# Documentation
#   - http://www.pygtk.org/pygtk2tutorial


import gtk
import os.path

from sys import argv
from subprocess import call


appname = "Python Presentation Manager"
appimage = os.path.expanduser("~/.icons/Tango/32x32/devices/video-display.png")


# Display presets; Name, Description, XRandR options
dispresets = {
    "LVDS Only"    : ["LVDS at 1280x800",
                      "--output LVDS --auto --output VGA --off --output TV --off"],
    "TV Clone"     : ["LVDS cloned on TV-Out at 1024x768",
                      "--output TV --mode 1024x768 --same-as LVDS"],
    "TV Extended"  : ["LVDS extended to TV-Out at 1024x768",
                      "--output TV --mode 1024x768 --right-of LVDS"],
    "TV Only"      : ["TV-Out at 1024x768",
                      "--output TV --mode 1024x768 --output LVDS --off"],
    "VGA Clone"    : ["LVDS cloned on VGA at 1280x1024",
                      "--output VGA --mode 1280x1024 --same-as LVDS"],
    "VGA Extended" : ["LVDS extended to VGA at 1280x1024",
                      "--output VGA --mode 1280x1024 --right-of LVDS"],
    "VGA Only"     : ["VGA at 1280x1024",
                      "--output VGA --mode 1280x1024 --output LVDS --off"],
    "VGA Wine"     : ["LVDS extended to VGA at 800x600",
                      "--output VGA --mode 800x600 --right-of LVDS"],
    # Used to initialise the TV set, omitted from the manager list
    "TV Init"      : ["TV initialisation string",
                      "--output TV --set TV_FORMAT PAL"]
}



class PyPres:
    presentation_icon  = [
      "/usr/share/icons/Tango/16x16/devices/video-display.png",
      os.path.expanduser("~/.icons/Tango/16x16/devices/video-display.png")
    ]

    def presentation_select_icon(self):
        for i in self.presentation_icon:
            if os.path.isfile(i):
                pypres_icon = gtk.gdk.pixbuf_new_from_file(i)
                break
            else: # Default to stock if Tango is unavailable
                pypres_icon = self.window.render_icon(gtk.STOCK_FULLSCREEN, gtk.ICON_SIZE_DIALOG)
        return pypres_icon

    def presentation_enable_display(self, preset):
        if "TV" in preset:
            # Initialise the TV set
            call("xrandr " + dispresets["TV Init"][1], shell=True)
        # Setup displays based on a preset
        call("xrandr " + dispresets[preset][1], shell=True)


    def __init__(self):
        # Create and setup a new window
        #  - titlebar, moveable, resizeable...
        #self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        #  - ontop, sticky, un-moveable, un-resizeable...
        self.window = gtk.Window(gtk.WINDOW_POPUP)
        # Window properties
        self.window.set_title(appname)
        self.window.set_border_width(5)
        self.window.set_size_request(310, 222)
        self.window.set_position(gtk.WIN_POS_CENTER)

        # Connect the destroy event to a handler
        self.window.connect("destroy", lambda x: gtk.main_quit())


        # Create a vertical container box with small padding
        self.vbox = gtk.VBox(False, 2)

        # Create an image
        self.image = gtk.Image()
        self.image.set_from_file(appimage)
        # Integrate it into the vbox
        self.vbox.pack_start(self.image)

        # Create a label notice
        self.label = gtk.Label(appname)
        # Integrate it into the vbox
        self.vbox.pack_start(self.label)


        # Create a scrolled window
        self.scrolledwin = gtk.ScrolledWindow()
        self.scrolledwin.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        self.scrolledwin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        # Integrate it into the vbox
        #  - child, expand, fill, padding
        self.vbox.pack_start(self.scrolledwin, True, True, 0)

        # Create a TreeView object
        liststore = self.create_listmodel()
        self.treeview = gtk.TreeView(liststore)
        self.treeview.connect("row-activated", self.on_clkactivated)
        # Change the bg color of every 2nd row
        self.treeview.set_rules_hint(True)

        # Add the TreeView to the scrolled window
        self.scrolledwin.add(self.treeview)
        # Create TreeView columns
        self.create_columns(self.treeview)

        # Add the vbox to the main window
        self.window.add(self.vbox)


        # Create a button box
        self.bbox = gtk.HButtonBox ()
        self.vbox.pack_start(self.bbox, False, False, 0)
        self.bbox.set_layout(gtk.BUTTONBOX_SPREAD)

        # OK button widget
        button = gtk.Button(stock=gtk.STOCK_OK)
        button.connect("clicked", self.on_btnactivated)
        self.bbox.add(button)

        # Close button widget
        button = gtk.Button(stock=gtk.STOCK_CANCEL)
        button.connect("clicked", lambda w: gtk.main_quit())
        self.bbox.add(button)
        # This is the default button
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()


        # Set window icon
        gtk.window_set_default_icon(self.presentation_select_icon())

        # Draw the presentation manager
        self.window.show_all()


    def create_listmodel(self):
        store = gtk.ListStore(str, str)
        # Built-in dictionary type specifically disclaims any
        # preservation of order
        sortedkeys = dispresets.keys()
        sortedkeys.sort()
        # But the list looks much nicer when sorted
        for key in sortedkeys:
            if key != "TV Init":
                store.append([key, dispresets[key][0]])
        return store

    def create_columns(self, treeview):
        # CellRendererText renders text into a gtk.TreeView cell
        cell = gtk.CellRendererText()
        # Create a presets column
        #  - title, cell_renderer, text
        column = gtk.TreeViewColumn("Preset", cell, text=0)
        # Column ID to sort
        column.set_sort_column_id(0)
        # Append the column to the treeview
        treeview.append_column(column)

        # Repeat for descriptions column
        cell = gtk.CellRendererText()
        column = gtk.TreeViewColumn("Description", cell, text=1)
        column.set_sort_column_id(1)
        treeview.append_column(column)


    def on_clkactivated(self, widget, row, col):
        # When a list item was selected by a double-click
        model = widget.get_model()
        self.presentation_enable_display(model[row][0])
        gtk.main_quit()

    def on_btnactivated(self, widget):
        # When a list item was selected and button OK was pressed
        selection = self.treeview.get_selection()
        model, list = selection.get_selected_rows()
        # Check if selection is not empty
        if list:
            self.presentation_enable_display(model[list[0][0]][0])
            gtk.main_quit()
        return



def main():
    gtk.main()
    return 0


if __name__ == "__main__":
    if len(argv) < 2:
        PyPres()
    else:
        raise SystemExit("Usage: %s\n * %s" % (os.path.split(argv[0])[1], appname))
    main()
