package hamster.ui.opt;

import hamster.GlobalSettings;
import hamster.data.TranslationLookup;
import hamster.gob.Tag;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.*;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import static hamster.GlobalSettings.*;

public class GameplayPanel extends Scrollport {
    public GameplayPanel(final UI ui) {
        super(OptionsWnd.PANEL_SIZE);
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping overall = new LinearGrouping(spacer, false, LinearGrouping.Direction.VERTICAL);
        final Grouping sys = new LinearGrouping(TranslationLookup.get("opt_gameplay_sys"), spacer, false);
        final Grouping lighting = new LinearGrouping(TranslationLookup.get("opt_gameplay_light"), spacer, false);
        final Grouping map = new LinearGrouping(TranslationLookup.get("opt_gameplay_map"), spacer, false);
        final Grouping cam = new LinearGrouping(TranslationLookup.get("opt_gameplay_cam"), spacer, false);
        final Grouping gob = new LinearGrouping(TranslationLookup.get("opt_gameplay_gob"), spacer, false);
        final Grouping animal = new LinearGrouping(TranslationLookup.get("opt_gameplay_animal"), spacer, false);
        final Grouping pf = new LinearGrouping(TranslationLookup.get("opt_gameplay_pf"), spacer, false);

        { //System
            sys.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_sys_debug"), GlobalSettings.DEBUG));
            sys.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_sys_fps"), SHOWFPS));
            sys.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_sys_stats"), GlobalSettings.SHOWSTATS));
            sys.pack();
            overall.add(sys);
        }
        { //Camera
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>(TranslationLookup.get("opt_gameplay_cam_type"), UI.scale(500), GlobalSettings.CAMERA,
                    (camera) -> MapView.MessageBus.send(new MapView.SetCamera(camera)));
            {
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_ortho"), "sortho");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_locked_ortho"), "ortho");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_free"), "worse");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_smooth_free"), "bad");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_follow"), "follow");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_topdown"), "topdown");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_fixator"), "fixator");
                rgrp.add(TranslationLookup.get("opt_gameplay_cam_freestyle"), "freestyle");
            }
            final Grouping freeg = new LinearGrouping(TranslationLookup.get("opt_gameplay_cam_free_set"), spacer, false);
            { //Free Cam Settings
                freeg.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_cam_free_reverse_x"), GlobalSettings.FREECAMREXAXIS));
                freeg.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_cam_free_reverse_y"), GlobalSettings.FREECAMREYAXIS));
                freeg.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_cam_free_lock_elev"), GlobalSettings.FREECAMLOCKELAV));
                freeg.pack();
            }

            cam.add(rgrp);
            cam.add(new IndirLabel(() -> String.format("%s%d", TranslationLookup.get("opt_gameplay_cam_projection"), GlobalSettings.CAMERAPROJFAR.get())));
            cam.add(new IndirHSlider(UI.scale(200), 5000, 50000, GlobalSettings.CAMERAPROJFAR,
                    (val) -> MapView.MessageBus.send(new MapView.CameraResized())));
            cam.add(freeg);
            cam.pack();
            overall.add(cam);
        }
        { // Lights
            lighting.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_light_nv"), NIGHTVISION));
            lighting.add(OptionsWnd.ColorPreWithLabel( TranslationLookup.get("opt_gameplay_light_nv_amb"), NVAMBIENTCOL));
            lighting.add(OptionsWnd.ColorPreWithLabel( TranslationLookup.get("opt_gameplay_light_nv_dif"), NVDIFFUSECOL));
            lighting.add(OptionsWnd.ColorPreWithLabel( TranslationLookup.get("opt_gameplay_light_nv_spc"), NVSPECCOL));
            lighting.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_light_dark"), DARKMODE));
            lighting.pack();
            overall.add(lighting);
        }
        { // Map related
            //Display related
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_map"), SHOWMAP, (val) -> MapView.MessageBus.send(new MapView.ToggleMap(val))));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_gob"), SHOWGOBS, (val) -> MapView.MessageBus.send(new MapView.ToggleGobs(val))));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_keep_gob"), KEEPGOBS));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_keep_grid"), KEEPGRIDS));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_skip"), SKIPLOADING));
            map.add(new IndirLabel(() -> String.format("%s%d", TranslationLookup.get("opt_gameplay_map_draw_dist"), DRAWGRIDRADIUS.get())));
            map.add(new IndirHSlider(200, 1, 30, DRAWGRIDRADIUS));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_flat"), FLATWORLD));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_weather"), SHOWWEATHER));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_allow_shaking"), ALLOWSHAKING));
            //Grid related
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_flav"), SHOWFLAVOBJS, (val) -> MapView.MessageBus.send(new MapView.ToggleFlavObjs(val))));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_tran"), SHOWTRANTILES, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            //Ocean related
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_show_water"), SHOWWATERSURF, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_colorize_ocean"), COLORIZEDEEPWATER, (val) -> MCache.MessageBus.send(new MCache.UpdateWaterTile())));
            map.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_gameplay_map_ocean_col"), DEEPWATERCOL, (val) -> MCache.MessageBus.send(new MCache.UpdateWaterTile())));
            //Cave related
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_short_cave"), GlobalSettings.SHORTCAVEWALLS, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            //Winter / Snow related
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_snow"), SHOWSNOW));
            map.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_map_snow_large"), LARGESNOWFLAKE, (val) -> MCache.MessageBus.send(new MCache.InvalidateAllGrids())));
            final IndirRadioGroup<Integer> sfs = map.add(new IndirRadioGroup<>(TranslationLookup.get("opt_gameplay_map_snow_fall_spd"), UI.scale(450), SNOWFALLSPEED));
            {
                sfs.add(TranslationLookup.get("opt_gameplay_map_snow_standard"), 1);
                sfs.add("5×", 2);
                sfs.add("50×", 50);
                sfs.add("100×", 100);
                sfs.add("1000×", 1000);
            }
            final IndirRadioGroup<Integer> sgs = map.add(new IndirRadioGroup<>(TranslationLookup.get("opt_gameplay_map_snow_gust_spd"), UI.scale(450), SNOWGUSTSPEED));
            {
                sgs.add(TranslationLookup.get("opt_gameplay_map_snow_standard"), 1);
                sgs.add("2×", 2);
                sgs.add("50×", 50);
                sgs.add("100×", 100);
                sgs.add("1000×", 1000);
            }
            final IndirRadioGroup<Integer> sd = map.add(new IndirRadioGroup<>(TranslationLookup.get("opt_gameplay_map_snow_density"), UI.scale(450), SNOWDENSITY));
            {
                sd.add(String.format("%s1s", TranslationLookup.get("opt_gameplay_map_snow_flake_every")), 1);
                sd.add(String.format("%s500ms", TranslationLookup.get("opt_gameplay_map_snow_flake_every")), 2);
                sd.add(String.format("%s200ms", TranslationLookup.get("opt_gameplay_map_snow_flake_every")), 5);
                sd.add(String.format("%s100ms", TranslationLookup.get("opt_gameplay_map_snow_flake_every")), 10);
                sd.add(String.format("%s10ms", TranslationLookup.get("opt_gameplay_map_snow_flake_every")), 100);
            }
            map.pack();
            overall.add(map);
        }
        { //Gob
            gob.add(new Label(TranslationLookup.get("opt_gameplay_gob_badkin")));
            gob.add(new IndirGroupSelector(GlobalSettings.BADKIN, BuddyWnd.gc));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_halo_urself"), GlobalSettings.SHOWPLAYERHALO));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_halo_players"), GlobalSettings.SHOWGOBHALO));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_halo_hearth"), GlobalSettings.SHOWGOBHALOONHEARTH));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_short_wall"), SHORTWALLS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_short_cupboards"), SHORTCUPBOARDS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_aggro"), GlobalSettings.COLORIZEAGGRO));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_drying"), GlobalSettings.COLORFULFARMES));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_tanning"), GlobalSettings.COLORFULTUBS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_cupboards"), GlobalSettings.COLORFULCUPBOARDS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_cheese"), GlobalSettings.COLORFULCHEESERACKS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_colorized_dust"), COLORFULDUST));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_dust_longer"), LONGLIVINGDUST));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_dust_larger"), LARGEDUSTSIZE));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_crop_stage"), GlobalSettings.SHOWCROPSTAGE));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_simple_crop"), GlobalSettings.SIMPLECROPS));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_speed"), SHOWGOBSPEED));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_damage"), GlobalSettings.SHOWGOBHP));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_player_dmg"), SHOWPLAYERDMG));
            gob.add(new IndirCheckBox(TranslationLookup.get("opt_gameplay_gob_player_path"), GlobalSettings.SHOWGOBPATH));
            gob.add(new IndirLabel(() -> String.format("%s%d", TranslationLookup.get("opt_gameplay_gob_path_width"), GlobalSettings.PATHWIDTH.get()), Text.std));
            gob.add(new IndirHSlider(200, 1, 8, GlobalSettings.PATHWIDTH));
            gob.add(OptionsWnd.BaseColorPreWithLabel(TranslationLookup.get("opt_gameplay_gob_player_path_col"), GlobalSettings.GOBPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel(TranslationLookup.get("opt_gameplay_gob_vehicle_path_col"), GlobalSettings.VEHPATHCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel(TranslationLookup.get("opt_gameplay_gob_hidden_col"), GlobalSettings.GOBHIDDENCOL));
            gob.add(OptionsWnd.BaseColorPreWithLabel(TranslationLookup.get("opt_gameplay_gob_hitbox_col"), GlobalSettings.GOBHITBOXCOL));
            gob.pack();
            overall.add(gob);
        }
        { //Animals
            animal.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_animal_forage"), GlobalSettings.FORAGEANIMALS));
            animal.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_animal_dmg"), SHOWANIMALDMG));
            animal.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_animal_speed"), SHOWANIMALSPEED));
            animal.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_animal_path"), GlobalSettings.SHOWANIMALPATH));
            animal.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_animal_radius"), GlobalSettings.SHOWANIMALRADIUS));
            animal.add(OptionsWnd.BaseColorPreWithLabel( TranslationLookup.get("opt_gameplay_animal_path_col"), GlobalSettings.ANIMALPATHCOL));
            animal.pack();
            overall.add(animal);
        }
        { //Pathfinding
            final IndirRadioGroup<Integer> rg = pf.add(new IndirRadioGroup<>( TranslationLookup.get("opt_gameplay_pf_tier"), UI.scale(450), GlobalSettings.PATHFINDINGTIER));
            {
                rg.add( TranslationLookup.get("opt_gameplay_pf_perfect"), 1);
                rg.add( TranslationLookup.get("opt_gameplay_pf_decent"), 2);
                rg.add( TranslationLookup.get("opt_gameplay_pf_fastest"), 3);
            }
            pf.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_pf_limit"), GlobalSettings.LIMITPATHFINDING));
            pf.add(new IndirCheckBox( TranslationLookup.get("opt_gameplay_pf_research"), GlobalSettings.RESEARCHUNTILGOAL));
            pf.pack();
            overall.add(pf);
        }

        overall.pack();
        add(overall);
        pack();
    }
}
