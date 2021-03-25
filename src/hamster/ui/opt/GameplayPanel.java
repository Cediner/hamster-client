package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirHSlider;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import static hamster.GlobalSettings.*;

public class GameplayPanel extends Scrollport {
    public GameplayPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping sys = new LinearGrouping("System Settings", spacer, false);
        final Grouping lighting = new LinearGrouping("Light Settings (Global)", spacer, false);
        final Grouping map = new LinearGrouping("Map Settings (Global)", spacer, false);
        final Grouping cam = new LinearGrouping("Camera Settings", spacer, false);
        final Grouping gob = new LinearGrouping("Gob Settings", spacer, false);
        final Grouping pf = new LinearGrouping("Pathfinding Settings", spacer, false);

        { //System
            sys.add(new IndirCheckBox("Debug Mode", GlobalSettings.DEBUG));
            sys.add(new IndirCheckBox("Display stats in top right", GlobalSettings.SHOWSTATS));
            sys.pack();
        }
        { // Lights
            lighting.add(new IndirCheckBox("Nightvision", NIGHTVISION));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Ambient: ", NVAMBIENTCOL));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Diffuse: ", NVDIFFUSECOL));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Specular: ", NVSPECCOL));
            lighting.add(new IndirCheckBox("Dark Mode (Restart client when changing this)", DARKMODE));
            lighting.pack();
        }
        { // Map related - TODO: probably makes more sense to be in Gameplay as i can attempt to trigger reload of grids, etc
            //Display related
            map.add(new IndirCheckBox("Show map", SHOWMAP, (val) -> ui.gui.map.toggleMap(val)));
            map.add(new IndirCheckBox("Show gobs", SHOWGOBS, (val) -> ui.gui.map.toggleGobs(val)));
            map.add(new IndirCheckBox("Keep gobs forever (Not implemented)", ui.gui.settings.KEEPGOBS));
            map.add(new IndirCheckBox("Keep grids forever (Not implemented)", ui.gui.settings.KEEPGRIDS));
            map.add(new IndirCheckBox("Skip loading (Not implemented)", SKIPLOADING));
            map.add(new IndirLabel(() -> String.format("Map grid draw distance: %d", DRAWGRIDRADIUS.get())));
            map.add(new IndirHSlider(200, 1, 30, DRAWGRIDRADIUS));
            map.add(new IndirCheckBox("Flatworld (Not implemented)", FLATWORLD));
            //Grid related
            map.add(new IndirCheckBox("Show Flavor Objects", SHOWFLAVOBJS, (val) -> ui.gui.map.terrain.toggleFlav(val)));
            map.add(new IndirCheckBox("Show Transition tiles (Requires reload of nearby grids)", SHOWTRANTILES));
            //Ocean related
            map.add(new IndirCheckBox("Colorize Deep Ocean tiles (Requires reload of nearby grids)", COLORIZEDEEPWATER));
            map.add(OptionsWnd.ColorPreWithLabel("Deep Ocean tile color (Requires client restart): ", DEEPWATERCOL));
            //Cave related
            map.add(new IndirCheckBox("Short cave walls (Requires reload of nearby grids)", GlobalSettings.SHORTCAVEWALLS));
            map.pack();
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
            { //Free Cam Settings TODO
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
            gob.add(new IndirCheckBox("Colorize Aggro'd Gobs", ui.gui.settings.COLORIZEAGGRO));
            gob.add(new IndirCheckBox("Colorize Drying Frames", ui.gui.settings.COLORFULFARMES));
            gob.add(new IndirCheckBox("Colorize Tanning Tubs", ui.gui.settings.COLORFULTUBS));
            gob.add(new IndirCheckBox("Colorize Cupboards", ui.gui.settings.COLORFULCUPBOARDS));
            gob.add(new IndirCheckBox("Colorize Cheese Racks", ui.gui.settings.COLORFULCHEESERACKS));
            gob.add(new IndirCheckBox("Colorize Cave dust (Global)", COLORFULDUST));
            gob.add(new IndirCheckBox("Cave dust last longer (Global)", LONGLIVINGDUST));
            gob.add(new IndirCheckBox("Make Cave dust larger (Global)", LARGEDUSTSIZE));
            gob.add(new IndirCheckBox("Show Crop Stage", ui.gui.settings.SHOWCROPSTAGE));
            gob.add(new IndirCheckBox("Show Simple Crops", ui.gui.settings.SIMPLECROPS));
            gob.add(new IndirCheckBox("Show Player HP/Armor damage (Not implemented)", ui.gui.settings.SHOWGOBPATH));
            gob.add(new IndirCheckBox("Show Player Paths (Not implemented)", ui.gui.settings.SHOWGOBPATH));
            gob.add(new IndirCheckBox("Show Animal Paths (Not implemented)", ui.gui.settings.SHOWANIMALPATH));
            gob.add(new IndirCheckBox("Show Animal Radius (Not implemented)", ui.gui.settings.SHOWANIMALRADIUS));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hidden color: ", ui.gui.settings.GOBHIDDENCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hitbox color: ", ui.gui.settings.GOBHITBOXCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Player Path color: ", ui.gui.settings.GOBPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Vehicle Path color: ", ui.gui.settings.VEHPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Animal color: ", ui.gui.settings.ANIMALPATHCOL));
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
        y += add(lighting, new Coord(0, y)).sz.y + spacer.y;
        y += add(map, new Coord(0, y)).sz.y + spacer.y;
        y += add(cam, new Coord(0, y)).sz.y + spacer.y;
        y += add(gob, new Coord(0, y)).sz.y + spacer.y;
        y += add(pf, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
