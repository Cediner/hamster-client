package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirHSlider;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.Coord;
import haven.UI;

public class UIPanel extends Scrollport {
    public UIPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping visibility = new GridGrouping("UI Visibility Settings", spacer, spacer.x, UI.scale(200), false);
        final Grouping minimap = new GridGrouping("Minimap Settings", spacer, spacer.x, UI.scale(200), false);
        final Grouping menu = new LinearGrouping("MenuGrid Settings", spacer, false);
        final Grouping meter = new LinearGrouping("Meter Settings", spacer, false);
        final Grouping inv = new LinearGrouping("Inventory Settings", spacer, false);
        final Grouping fmenu = new LinearGrouping("Flowermenu Settings", spacer, false);

        { //visibility
            visibility.add(new IndirCheckBox("Show Player Avatar", ui.gui.settings.SHOWPLAVA, (val) -> ui.gui.portrait.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Speed", ui.gui.settings.SHOWSPEED, (val) -> ui.gui.speed.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Health", ui.gui.settings.SHOWHEALTH, (val) -> ui.gui.hp.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Energy", ui.gui.settings.SHOWENERGY, (val) -> ui.gui.energy.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Stamina", ui.gui.settings.SHOWSTAM, (val) -> ui.gui.stam.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Chat Window", ui.gui.settings.SHOWCHAT, (val) -> ui.gui.chatwnd.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Calendar", ui.gui.settings.SHOWCAL, val -> ui.gui.cal.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Sessions Display", ui.gui.settings.SHOWSESSIONS, val -> ui.root.sessionDisplay.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 1", ui.gui.settings.SHOWHOTBAR1, (val) -> ui.gui.hotbar1.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 2", ui.gui.settings.SHOWHOTBAR2, (val) -> ui.gui.hotbar2.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 3", ui.gui.settings.SHOWHOTBAR3, (val) -> ui.gui.hotbar3.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Mini Inventory", ui.gui.settings.SHOWMINIINV, val -> ui.gui.mminv.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Mini Equipment", ui.gui.settings.SHOWMINIEQU, val -> ui.gui.mmequ.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Left/Right Hand Slots", ui.gui.settings.SHOWLRSLOTS, val -> ui.gui.lrhandview.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Study Window", ui.gui.settings.SHOWSTUDY, val -> ui.gui.study.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Inventory on Login", ui.gui.settings.SHOWINVONLOGIN));
            visibility.add(new IndirCheckBox("Show Belt on Login", ui.gui.settings.SHOWBELTONLOGIN));
            visibility.add(new IndirCheckBox("Show Experience Windows", ui.gui.settings.SHOWEXPWND));
            visibility.pack();
        }
        { //minimap
            minimap.add(new IndirCheckBox("Show Gobs", GlobalSettings.SHOWMMGOBS));
            minimap.add(new IndirCheckBox("Show Marker Names", GlobalSettings.SHOWMMMARKERNAMES));
            minimap.add(new IndirCheckBox("Show Gob Names", GlobalSettings.SHOWMMGOBNAMES));
            minimap.add(new IndirCheckBox("Show Markers", GlobalSettings.SHOWMMMARKERS));
            minimap.add(new IndirCheckBox("Show Small Markers", GlobalSettings.SMALLMMMARKERS));
            minimap.add(new IndirCheckBox("Show Placed Markers", GlobalSettings.SHOWPMARKERS));
            minimap.add(new IndirCheckBox("Show Natural Markers", GlobalSettings.SHOWNMARKERS));
            minimap.add(new IndirCheckBox("Show Custom Markers", GlobalSettings.SHOWCMARKERS));
            minimap.add(new IndirCheckBox("Show Linked Markers", GlobalSettings.SHOWLMARKERS));
            minimap.add(new IndirCheckBox("Show Kingdom Markers", GlobalSettings.SHOWKMARKERS));
            minimap.add(new IndirCheckBox("Show Kingdom Radius", GlobalSettings.SHOWKMARKERRAD));
            minimap.add(new IndirCheckBox("Show Village Markers", GlobalSettings.SHOWVMARKERS));
            minimap.add(new IndirCheckBox("Show Village Radius", GlobalSettings.SHOWVMARKERRAD));
            minimap.add(new IndirCheckBox("Show Village Names", GlobalSettings.SHOWVMARKERTIPS));
            minimap.add(OptionsWnd.ColorPreWithLabel("Queued Path Color: ", GlobalSettings.MMPATHCOL));
            minimap.pack();
        }
        { //menu
            menu.add(new IndirLabel(() -> String.format("MenuGrid Columns: %d", ui.gui.settings.MENUGRIDSIZEX.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, ui.gui.settings.MENUGRIDSIZEX, (val) -> {
                if(ui.gui.menu != null) {
                    ui.gui.menu.updlayoutsize();
                }
            }));
            menu.add(new IndirLabel(() -> String.format("MenuGrid Rows: %d", ui.gui.settings.MENUGRIDSIZEY.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, ui.gui.settings.MENUGRIDSIZEY, (val) -> {
                if(ui.gui.menu != null) {
                    ui.gui.menu.updlayoutsize();
                }
            }));
            menu.pack();
        }
        { //mods
            meter.add(new IndirCheckBox("Alternate Meter Display", ui.gui.settings.BIGSIMPLEMETERS));
            meter.pack();
        }
        { //Inventory
            inv.add(new IndirCheckBox("Show Item Quality", ui.gui.settings.SHOWITEMQ));
            inv.add(new IndirCheckBox("Show Item Wear Bar", ui.gui.settings.SHOWITEMWEAR));
            inv.add(new IndirCheckBox("Show Item Contents Bar", ui.gui.settings.SHOWITEMCONT));
            inv.add(new IndirCheckBox("Always show longtip on items", ui.gui.settings.ALWAYSITEMLONGTIPS));
            inv.add(new IndirCheckBox("Use special mousebind when dropping held items in water", ui.gui.settings.WATERDROPITEMCTRL));
            inv.pack();
        }
        { //Flowermenu
            fmenu.add(new IndirCheckBox("Quick Flowermenu", ui.gui.settings.QUICKFLMENU));
            fmenu.add(new IndirCheckBox("Don't close Flowermenu on clicks", ui.gui.settings.KEEPFLOPEN));
            fmenu.pack();
        }

        int y = 0;

        y += add(visibility, new Coord(0, y)).sz.y + spacer.y;
        y += add(minimap, new Coord(0, y)).sz.y + spacer.y;
        y += add(menu, new Coord(0, y)).sz.y + spacer.y;
        y += add(meter, new Coord(0, y)).sz.y + spacer.y;
        y += add(inv, new Coord(0, y)).sz.y + spacer.y;
        y += add(fmenu, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
