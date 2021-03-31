package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.*;
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
        final Grouping animal = new LinearGrouping("Animal Settings", spacer, false);
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
        { // Map related
            //Display related
            map.add(new IndirCheckBox("Show map", SHOWMAP, (val) -> ui.gui.map.toggleMap(val)));
            map.add(new IndirCheckBox("Show gobs", SHOWGOBS, (val) -> ui.gui.map.toggleGobs(val)));
            map.add(new IndirCheckBox("Keep gobs forever (Use with caution)", KEEPGOBS));
            map.add(new IndirCheckBox("Keep grids forever (Use with caution)", KEEPGRIDS));
            map.add(new IndirCheckBox("Skip loading", SKIPLOADING));
            map.add(new IndirLabel(() -> String.format("Map grid draw distance: %d", DRAWGRIDRADIUS.get())));
            map.add(new IndirHSlider(200, 1, 30, DRAWGRIDRADIUS));
            map.add(new IndirCheckBox("Flatworld (Not implemented)", FLATWORLD));
            //Grid related
            map.add(new IndirCheckBox("Show Flavor Objects", SHOWFLAVOBJS, (val) -> ui.gui.map.terrain.toggleFlav(val)));
            map.add(new IndirCheckBox("Show Transition tiles", SHOWTRANTILES, (val) -> ui.sess.glob.map.invalidateAll()));
            //Ocean related
            map.add(new IndirCheckBox("Show water surface top", SHOWWATERSURF, (val) -> ui.sess.glob.map.invalidateAll()));
            map.add(new IndirCheckBox("Colorize Deep Ocean tiles", COLORIZEDEEPWATER, (val) -> {
                ui.sess.glob.map.updateWaterTiles();
                ui.sess.glob.map.invalidateAll();
            }));
            map.add(OptionsWnd.ColorPreWithLabel("Deep Ocean tile color: ", DEEPWATERCOL, (val) -> {
                ui.sess.glob.map.updateWaterTiles();
                ui.sess.glob.map.invalidateAll();
            }));
            //Cave related
            map.add(new IndirCheckBox("Short cave walls", GlobalSettings.SHORTCAVEWALLS, (val) -> ui.sess.glob.map.invalidateAll()));
            map.pack();
        }
        { //Camera
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Camera Type", UI.scale(500), GlobalSettings.CAMERA, (camera) -> {
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
                freeg.add(new IndirCheckBox("Reverse X Axis for Free Cam", GlobalSettings.FREECAMREXAXIS));
                freeg.add(new IndirCheckBox("Reverse Y Axis for Free Cam", GlobalSettings.FREECAMREYAXIS));
                freeg.add(new IndirCheckBox("Free Cam lock elevation", GlobalSettings.FREECAMLOCKELAV));
                freeg.pack();
            }

            cam.add(rgrp);
            cam.add(new IndirLabel(() -> String.format("Camera Projection: %d", GlobalSettings.CAMERAPROJFAR.get())));
            cam.add(new IndirHSlider(UI.scale(200), 5000, 50000, GlobalSettings.CAMERAPROJFAR, (val) -> ui.gui.map.camera.resized()));
            cam.add(freeg);
            cam.pack();
        }
        { //Gob
            gob.add(new Label("Bad Kin Group:"));
            gob.add(new IndirGroupSelector(GlobalSettings.BADKIN, BuddyWnd.gc));
            gob.add(new IndirCheckBox("Show halo on players", GlobalSettings.SHOWGOBHALO));
            gob.add(new IndirCheckBox("Show halo on players on hearth", GlobalSettings.SHOWGOBHALOONHEARTH));
            gob.add(new IndirCheckBox("Colorize Aggro'd Gobs", GlobalSettings.COLORIZEAGGRO));
            gob.add(new IndirCheckBox("Colorize Drying Frames", GlobalSettings.COLORFULFARMES));
            gob.add(new IndirCheckBox("Colorize Tanning Tubs", GlobalSettings.COLORFULTUBS));
            gob.add(new IndirCheckBox("Colorize Cupboards", GlobalSettings.COLORFULCUPBOARDS));
            gob.add(new IndirCheckBox("Colorize Cheese Racks", GlobalSettings.COLORFULCHEESERACKS));
            gob.add(new IndirCheckBox("Colorize Cave dust (Global)", COLORFULDUST));
            gob.add(new IndirCheckBox("Cave dust last longer (Global)", LONGLIVINGDUST));
            gob.add(new IndirCheckBox("Make Cave dust larger (Global)", LARGEDUSTSIZE));
            gob.add(new IndirCheckBox("Show Crop Stage", GlobalSettings.SHOWCROPSTAGE));
            gob.add(new IndirCheckBox("Show Simple Crops", GlobalSettings.SIMPLECROPS));
            gob.add(new IndirCheckBox("Show Gob damage", GlobalSettings.SHOWGOBHP));
            gob.add(new IndirCheckBox("Show Player Paths", GlobalSettings.SHOWGOBPATH));
            gob.add(new IndirLabel(() -> String.format("Path Width: %d", GlobalSettings.PATHWIDTH.get()), Text.std));
            gob.add(new IndirHSlider(200, 1, 8, GlobalSettings.PATHWIDTH));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Player Path color (self): ", GlobalSettings.GOBPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Vehicle Path color: ", GlobalSettings.VEHPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hidden color: ", GlobalSettings.GOBHIDDENCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hitbox color: ", GlobalSettings.GOBHITBOXCOL));
            gob.pack();
        }
        { //Animals
            animal.add(new IndirCheckBox("Forage small animals with keybind", GlobalSettings.FORAGEANIMALS));
            animal.add(new IndirCheckBox("Show Animal Paths", GlobalSettings.SHOWANIMALPATH));
            animal.add(new IndirCheckBox("Show Dangerous Animal Radius", GlobalSettings.SHOWANIMALRADIUS));
            animal.add(OptionsWnd.BaseColorPreWithLabel("Animal Path color: ", GlobalSettings.ANIMALPATHCOL));
            animal.pack();
        }
        { //Pathfinding
            final IndirRadioGroup<Integer> rg = pf.add(new IndirRadioGroup<>("Pathfinding Tier", UI.scale(450), GlobalSettings.PATHFINDINGTIER));
            {
                rg.add("Perfect", 1);
                rg.add("Decent", 2);
                rg.add("Fastest", 3);
            }
            pf.add(new IndirCheckBox("Limit pathfinding to view distance", GlobalSettings.LIMITPATHFINDING));
            pf.add(new IndirCheckBox("Re-search goal until reached", GlobalSettings.RESEARCHUNTILGOAL));
            pf.pack();
        }

        int y = 0;

        y += add(cam, new Coord(0, y)).sz.y + spacer.y;
        y += add(sys, new Coord(0, y)).sz.y + spacer.y;
        y += add(lighting, new Coord(0, y)).sz.y + spacer.y;
        y += add(map, new Coord(0, y)).sz.y + spacer.y;
        y += add(gob, new Coord(0, y)).sz.y + spacer.y;
        y += add(animal, new Coord(0, y)).sz.y + spacer.y;
        add(pf, new Coord(0, y));
        pack();
    }
}
