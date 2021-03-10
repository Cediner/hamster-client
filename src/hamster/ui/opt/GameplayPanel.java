package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

public class GameplayPanel extends Scrollport {
    public GameplayPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping sys = new LinearGrouping("System Settings", spacer, false);
        final Grouping cam = new LinearGrouping("Camera Settings", spacer, false);
        final Grouping gob = new LinearGrouping("Gob Settings", spacer, false);
        final Grouping pf = new LinearGrouping("Pathfinding Settings", spacer, false);

        { //System
            sys.add(new IndirCheckBox("Debug Mode", GlobalSettings.DEBUG));
            sys.add(new IndirCheckBox("Display stats in top right", GlobalSettings.SHOWSTATS));
            sys.pack();
        }
        { //Camera
            final Coord c = new Coord(0, 0);
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Camera Type", UI.scale(500), ui.gui.settings.CAMERA, (camera) -> {
                if(ui.gui != null) {
                    ui.gui.map.setcam(camera);
                }
            });
            {
                rgrp.add("Ortho Cam", "sortho");
                rgrp.add("Angle Locked Ortho Cam", "ortho");
                rgrp.add("Non-smoothed Free Cam", "worse");
                rgrp.add("Smoothed Free Cam", "bad");
                rgrp.add("Follow Cam", "follow");
                rgrp.add("Top Down Cam", "topdown");
                rgrp.add("Fixator", "fixator");
                rgrp.add("Freestyle", "freestyle");
            }
            final Grouping freeg = new LinearGrouping("Free Cam Settings", spacer, false);
            { //Free Cam Settings
                freeg.add(new IndirCheckBox("Reverse X Axis for Free Cam", ui.gui.settings.FREECAMREXAXIS));
                freeg.add(new IndirCheckBox("Reverse Y Axis for Free Cam", ui.gui.settings.FREECAMREYAXIS));
                freeg.add(new IndirCheckBox("Free Cam lock elevation", ui.gui.settings.FREECAMLOCKELAV));
                freeg.pack();
            }

            cam.add(rgrp);
            cam.add(freeg);
            cam.pack();
        }
        { //Gob
            gob.add(new IndirCheckBox("Show halo on players", ui.gui.settings.SHOWGOBHALO));
            gob.add(new IndirCheckBox("Show halo on players on hearth", ui.gui.settings.SHOWGOBHALOONHEARTH));
            gob.add(new IndirCheckBox("Colorize Drying Frames", ui.gui.settings.COLORFULFARMES));
            gob.add(new IndirCheckBox("Colorize Tanning Tubs", ui.gui.settings.COLORFULTUBS));
            gob.add(new IndirCheckBox("Colorize Cupboards", ui.gui.settings.COLORFULCUPBOARDS));
            gob.add(new IndirCheckBox("Colorize Cheese Racks", ui.gui.settings.COLORFULCHEESERACKS));
            gob.add(new IndirCheckBox("Show Crop Stage", ui.gui.settings.SHOWCROPSTAGE));
            gob.add(new IndirCheckBox("Show Simple Crops", ui.gui.settings.SIMPLECROPS));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hidden color: ", ui.gui.settings.GOBHIDDENCOL));
            gob.pack();
        }
        { //Pathfinding
            final IndirRadioGroup<Integer> rg = pf.add(new IndirRadioGroup<>("Pathfinding Tier", UI.scale(450), ui.gui.settings.PATHFINDINGTIER));
            {
                rg.add("Perfect", 1);
                rg.add("Decent", 2);
                rg.add("Fastest", 3);
            }
            pf.add(new IndirCheckBox("Limit pathfinding to view distance", ui.gui.settings.LIMITPATHFINDING));
            pf.add(new IndirCheckBox("Re-search goal until reached", ui.gui.settings.RESEARCHUNTILGOAL));
            pf.pack();
        }

        int y = 0;

        y += add(sys, new Coord(0, y)).sz.y + spacer.y;
        y += add(cam, new Coord(0, y)).sz.y + spacer.y;
        y += add(gob, new Coord(0, y)).sz.y + spacer.y;
        y += add(pf, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
