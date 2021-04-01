package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.gob.Tag;
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

        final Grouping overall = new LinearGrouping(spacer, false, LinearGrouping.Direction.VERTICAL);
        final Grouping sys = new LinearGrouping("System Settings", spacer, false);
        final Grouping lighting = new LinearGrouping("Light Settings", spacer, false);
        final Grouping map = new LinearGrouping("Map Settings", spacer, false);
        final Grouping cam = new LinearGrouping("Camera Settings", spacer, false);
        final Grouping gob = new LinearGrouping("Gob Settings", spacer, false);
        final Grouping animal = new LinearGrouping("Animal Settings", spacer, false);
        final Grouping pf = new LinearGrouping("Pathfinding Settings", spacer, false);

        { //System
            sys.add(new IndirCheckBox("Debug Mode", GlobalSettings.DEBUG));
            sys.add(new IndirCheckBox("Display stats in top right", GlobalSettings.SHOWSTATS));
            sys.pack();
            overall.add(sys);
        }
        { //Camera
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Camera Type", UI.scale(500), GlobalSettings.CAMERA,
                    (camera) -> MapView.MessageBus.send(new MapView.SetCamera(camera)));
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
            cam.add(new IndirHSlider(UI.scale(200), 5000, 50000, GlobalSettings.CAMERAPROJFAR,
                    (val) -> MapView.MessageBus.send(new MapView.CameraResized())));
            cam.add(freeg);
            cam.pack();
            overall.add(cam);
        }
        { // Lights
            lighting.add(new IndirCheckBox("Nightvision", NIGHTVISION));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Ambient: ", NVAMBIENTCOL));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Diffuse: ", NVDIFFUSECOL));
            lighting.add(OptionsWnd.ColorPreWithLabel("Nightvision Specular: ", NVSPECCOL));
            lighting.add(new IndirCheckBox("Dark Mode (Restart client when changing this)", DARKMODE));
            lighting.pack();
            overall.add(lighting);
        }
        { // Map related
            //Display related
            map.add(new IndirCheckBox("Show map", SHOWMAP, (val) -> MapView.MessageBus.send(new MapView.ToggleMap(val))));
            map.add(new IndirCheckBox("Show gobs", SHOWGOBS, (val) -> MapView.MessageBus.send(new MapView.ToggleGobs(val))));
            map.add(new IndirCheckBox("Keep gobs forever (Use with caution)", KEEPGOBS));
            map.add(new IndirCheckBox("Keep grids forever (Use with caution)", KEEPGRIDS));
            map.add(new IndirCheckBox("Skip loading", SKIPLOADING));
            map.add(new IndirLabel(() -> String.format("Map grid draw distance: %d", DRAWGRIDRADIUS.get())));
            map.add(new IndirHSlider(200, 1, 30, DRAWGRIDRADIUS));
            map.add(new IndirCheckBox("Flatworld (Not implemented)", FLATWORLD));
            //Grid related
            map.add(new IndirCheckBox("Show Flavor Objects", SHOWFLAVOBJS, (val) -> MapView.MessageBus.send(new MapView.ToggleFlavObjs(val))));
            map.add(new IndirCheckBox("Show Transition tiles", SHOWTRANTILES, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            //Ocean related
            map.add(new IndirCheckBox("Show water surface top", SHOWWATERSURF, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            map.add(new IndirCheckBox("Colorize Deep Ocean tiles", COLORIZEDEEPWATER, (val) -> MCache.MessageBus.send(new MCache.UpdateWaterTile())));
            map.add(OptionsWnd.ColorPreWithLabel("Deep Ocean tile color: ", DEEPWATERCOL, (val) -> MCache.MessageBus.send(new MCache.UpdateWaterTile())));
            //Cave related
            map.add(new IndirCheckBox("Short cave walls", GlobalSettings.SHORTCAVEWALLS, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            map.pack();
            overall.add(map);
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
            gob.add(new IndirCheckBox("Colorize Cave dust", COLORFULDUST));
            gob.add(new IndirCheckBox("Cave dust last longer", LONGLIVINGDUST));
            gob.add(new IndirCheckBox("Make Cave dust larger", LARGEDUSTSIZE));
            gob.add(new IndirCheckBox("Show Crop Stage", GlobalSettings.SHOWCROPSTAGE));
            gob.add(new IndirCheckBox("Show Simple Crops (Requires reload of gobs in view)", GlobalSettings.SIMPLECROPS));
            gob.add(new IndirCheckBox("Show Player Speed", SHOWGOBSPEED));
            gob.add(new IndirCheckBox("Show Gob damage", GlobalSettings.SHOWGOBHP));
            gob.add(new IndirCheckBox("Show Player Paths", GlobalSettings.SHOWGOBPATH));
            gob.add(new IndirLabel(() -> String.format("Path Width: %d", GlobalSettings.PATHWIDTH.get()), Text.std));
            gob.add(new IndirHSlider(200, 1, 8, GlobalSettings.PATHWIDTH));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Player Path color (self): ", GlobalSettings.GOBPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Vehicle Path color: ", GlobalSettings.VEHPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hidden color: ", GlobalSettings.GOBHIDDENCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel("Hitbox color: ", GlobalSettings.GOBHITBOXCOL));
            gob.pack();
            overall.add(gob);
        }
        { //Animals
            animal.add(new IndirCheckBox("Forage small animals with keybind", GlobalSettings.FORAGEANIMALS));
            animal.add(new IndirCheckBox("Show Animal Speed", SHOWANIMALSPEED));
            animal.add(new IndirCheckBox("Show Animal Paths", GlobalSettings.SHOWANIMALPATH));
            animal.add(new IndirCheckBox("Show Dangerous Animal Radius", GlobalSettings.SHOWANIMALRADIUS));
            animal.add(OptionsWnd.BaseColorPreWithLabel("Animal Path color: ", GlobalSettings.ANIMALPATHCOL));
            animal.pack();
            overall.add(animal);
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
            overall.add(pf);
        }

        overall.pack();
        add(overall);
        pack();
    }
}
