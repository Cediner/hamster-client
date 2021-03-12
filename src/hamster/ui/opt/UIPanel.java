package hamster.ui.opt;

import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
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
            visibility.add(new IndirCheckBox("Show Study Window", ui.gui.settings.SHOWSTUDY));
            visibility.add(new IndirCheckBox("Show Inventory on Login", ui.gui.settings.SHOWINVONLOGIN));
            visibility.add(new IndirCheckBox("Show Belt on Login", ui.gui.settings.SHOWBELTONLOGIN));
            visibility.pack();
        }
        { //minimap
            minimap.add(new IndirCheckBox("Show Gobs", ui.gui.settings.SHOWMMGOBS));
            minimap.add(new IndirCheckBox("Show Marker Names", ui.gui.settings.SHOWMMMARKERNAMES));
            minimap.add(new IndirCheckBox("Show Gob Names", ui.gui.settings.SHOWMMGOBNAMES));
            minimap.add(new IndirCheckBox("Show Markers", ui.gui.settings.SHOWMMMARKERS));
            minimap.add(new IndirCheckBox("Show Small Markers", ui.gui.settings.SMALLMMMARKERS));
            minimap.add(new IndirCheckBox("Show Placed Markers", ui.gui.settings.SHOWPMARKERS));
            minimap.add(new IndirCheckBox("Show Natural Markers", ui.gui.settings.SHOWNMARKERS));
            minimap.add(new IndirCheckBox("Show Custom MArkers", ui.gui.settings.SHOWCMARKERS));
            minimap.add(new IndirCheckBox("Show Linked Markers", ui.gui.settings.SHOWLMARKERS));
            minimap.add(new IndirCheckBox("Show Kingdom Markers", ui.gui.settings.SHOWKMARKERS));
            minimap.add(new IndirCheckBox("Show Kingdom Radius", ui.gui.settings.SHOWKMARKERRAD));
            minimap.add(new IndirCheckBox("Show Village Markers", ui.gui.settings.SHOWVMARKERS));
            minimap.add(new IndirCheckBox("Show Village Radius", ui.gui.settings.SHOWVMARKERRAD));
            minimap.add(new IndirCheckBox("Show Village Names", ui.gui.settings.SHOWVMARKERTIPS));
            minimap.add(OptionsWnd.ColorPreWithLabel("Queued Path Color", ui.gui.settings.MMPATHCOL));
            minimap.pack();
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
        y += add(meter, new Coord(0, y)).sz.y + spacer.y;
        y += add(inv, new Coord(0, y)).sz.y + spacer.y;
        y += add(fmenu, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
