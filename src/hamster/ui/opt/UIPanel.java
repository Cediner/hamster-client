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
import haven.MenuGrid;
import haven.UI;

public class UIPanel extends Scrollport {
    public UIPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping overall = new LinearGrouping(spacer, false, LinearGrouping.Direction.VERTICAL);
        final Grouping visibility = new GridGrouping("UI Visibility Settings", spacer, spacer.x, UI.scale(200), false);
        final Grouping minimap = new GridGrouping("Minimap Settings", spacer, spacer.x, UI.scale(200), false);
        final Grouping menu = new LinearGrouping("MenuGrid Settings", spacer, false);
        final Grouping meter = new LinearGrouping("Meter Settings", spacer, false);
        final Grouping inv = new LinearGrouping("Inventory Settings", spacer, false);
        final Grouping fmenu = new LinearGrouping("Flowermenu Settings", spacer, false);

        { //visibility
            visibility.add(new IndirCheckBox("Show Player Avatar", GlobalSettings.SHOWPLAVA, (val) -> ui.gui.portrait.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Speed", GlobalSettings.SHOWSPEED, (val) -> ui.gui.speed.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Health", GlobalSettings.SHOWHEALTH, (val) -> ui.gui.hp.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Energy", GlobalSettings.SHOWENERGY, (val) -> ui.gui.energy.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Player Stamina", GlobalSettings.SHOWSTAM, (val) -> ui.gui.stam.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Chat Window", GlobalSettings.SHOWCHAT, (val) -> ui.gui.chatwnd.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Calendar", GlobalSettings.SHOWCAL, val -> ui.gui.cal.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Sessions Display", GlobalSettings.SHOWSESSIONS, val -> ui.root.sessionDisplay.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 1", GlobalSettings.SHOWHOTBAR1, (val) -> ui.gui.hotbar1.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 2", GlobalSettings.SHOWHOTBAR2, (val) -> ui.gui.hotbar2.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Hotbar 3", GlobalSettings.SHOWHOTBAR3, (val) -> ui.gui.hotbar3.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Mini Inventory", GlobalSettings.SHOWMINIINV, val -> ui.gui.mminv.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Mini Equipment", GlobalSettings.SHOWMINIEQU, val -> ui.gui.mmequ.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Left/Right Hand Slots", GlobalSettings.SHOWLRSLOTS, val -> ui.gui.lrhandview.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Study Window", GlobalSettings.SHOWSTUDY, val -> ui.gui.study.setVisible(val)));
            visibility.add(new IndirCheckBox("Show Inventory on Login", GlobalSettings.SHOWINVONLOGIN));
            visibility.add(new IndirCheckBox("Show Belt on Login", GlobalSettings.SHOWBELTONLOGIN));
            visibility.add(new IndirCheckBox("Show Experience Windows", GlobalSettings.SHOWEXPWND));
            visibility.pack();
            overall.add(visibility);
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
            overall.add(minimap);
        }
        { //Inventory
            inv.add(new IndirCheckBox("Show Item Quality", GlobalSettings.SHOWITEMQ));
            inv.add(new IndirCheckBox("Show Item Wear Bar", GlobalSettings.SHOWITEMWEAR));
            inv.add(new IndirCheckBox("Show Item Contents Bar", GlobalSettings.SHOWITEMCONT));
            inv.add(new IndirCheckBox("Always show longtip on items", GlobalSettings.ALWAYSITEMLONGTIPS));
            inv.add(new IndirCheckBox("Use special mousebind when dropping held items in water", GlobalSettings.WATERDROPITEMCTRL));
            inv.pack();
            overall.add(inv);
        }
        { // Menu Grid
            menu.add(new IndirLabel(() -> String.format("MenuGrid Columns: %d", GlobalSettings.MENUGRIDSIZEX.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, GlobalSettings.MENUGRIDSIZEX, (val) -> MenuGrid.MessageBus.send(new MenuGrid.UpdateLayout())));
            menu.add(new IndirLabel(() -> String.format("MenuGrid Rows: %d", GlobalSettings.MENUGRIDSIZEY.get())));
            menu.add(new IndirHSlider(UI.scale(200), 4, 16, GlobalSettings.MENUGRIDSIZEY, (val) -> MenuGrid.MessageBus.send(new MenuGrid.UpdateLayout())));
            menu.pack();
            overall.add(menu);
        }
        { //Flowermenu
            fmenu.add(new IndirCheckBox("Quick Flowermenu", GlobalSettings.QUICKFLMENU));
            fmenu.add(new IndirCheckBox("Don't close Flowermenu on clicks", GlobalSettings.KEEPFLOPEN));
            fmenu.pack();
            overall.add(fmenu);
        }
        { // Meter
            meter.add(new IndirCheckBox("Alternate Meter Display", GlobalSettings.BIGSIMPLEMETERS));
            meter.pack();
            overall.add(meter);
        }

        overall.pack();
        add(overall);
        pack();
    }
}
