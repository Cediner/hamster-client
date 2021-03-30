package hamster;

import haven.Coord;
import haven.render.BaseColor;

import java.awt.*;

/**
 * Session based settings based off account:character name
 */
public class SessionSettings {
    ////Display / Graphical
    public final IndirSetting<Boolean> SHOWPCLAIM;
    public final IndirSetting<Boolean> SHOWVCLAIM;
    public final IndirSetting<Boolean> SHOWKCLAIM;

    ////Minimap
    public final IndirSetting<Boolean> SHOWMMGOBS;
    public final IndirSetting<Boolean> SHOWMMMARKERNAMES;
    public final IndirSetting<Boolean> SHOWMMGOBNAMES;
    public final IndirSetting<Boolean> SHOWMMMARKERS;
    public final IndirSetting<Boolean> SMALLMMMARKERS;
    public final IndirSetting<Boolean> SHOWPMARKERS;
    public final IndirSetting<Boolean> SHOWNMARKERS;
    public final IndirSetting<Boolean> SHOWCMARKERS;
    public final IndirSetting<Boolean> SHOWLMARKERS;
    public final IndirSetting<Boolean> SHOWKMARKERS;
    public final IndirSetting<Boolean> SHOWKMARKERRAD;
    public final IndirSetting<Boolean> SHOWVMARKERS;
    public final IndirSetting<Boolean> SHOWVMARKERRAD;
    public final IndirSetting<Boolean> SHOWVMARKERTIPS;

    public final IndirSetting<Coord> MMMEMSIZEONE;
    public final IndirSetting<Coord> MMMEMPOSONE ;
    public final IndirSetting<Coord> MMMEMSIZETWO;
    public final IndirSetting<Coord> MMMEMPOSTWO;
    public final IndirSetting<Boolean> MMSHOWGRID;
    public final IndirSetting<Boolean> MMSHOWVIEW;
    public final IndirSetting<Color> MMPATHCOL;

    ////Gob
    public final IndirSetting<Integer> BADKIN;
    public final IndirSetting<Boolean> COLORFULFARMES;
    public final IndirSetting<Boolean> COLORFULTUBS;
    public final IndirSetting<Boolean> COLORFULCUPBOARDS;
    public final IndirSetting<Boolean> COLORFULCHEESERACKS;
    public final IndirSetting<Boolean> SHOWCROPSTAGE;
    public final IndirSetting<Boolean> SIMPLECROPS;
    public final IndirSetting<BaseColor> GOBHIDDENCOL;
    public final IndirSetting<BaseColor> GOBHITBOXCOL;
    public final IndirSetting<Boolean> SHOWGOBHALO;
    public final IndirSetting<Boolean> SHOWGOBHALOONHEARTH;
    public final IndirSetting<Boolean> SHOWGOBHP;
    public final IndirSetting<Integer> PATHWIDTH;
    public final IndirSetting<Boolean> SHOWGOBPATH;
    public final IndirSetting<Boolean> SHOWANIMALPATH;
    public final IndirSetting<Boolean> SHOWANIMALRADIUS;
    public final IndirSetting<BaseColor> GOBPATHCOL;
    public final IndirSetting<BaseColor> ANIMALPATHCOL;
    public final IndirSetting<BaseColor> VEHPATHCOL;

    ////Camera
    public final IndirSetting<String> CAMERA;
    public final IndirSetting<Integer> CAMERAPROJFAR;
    public final IndirSetting<Boolean> FREECAMREXAXIS;
    public final IndirSetting<Boolean> FREECAMREYAXIS;
    public final IndirSetting<Boolean> FREECAMLOCKELAV;

    ////Pathfinding
    public final IndirSetting<Integer> PATHFINDINGTIER;
    public final IndirSetting<Boolean> LIMITPATHFINDING;
    public final IndirSetting<Boolean> RESEARCHUNTILGOAL;

    ////UI
    public final IndirSetting<Boolean> SHOWPLAVA;
    public final IndirSetting<Boolean> SHOWSPEED;
    public final IndirSetting<Boolean> SHOWHEALTH;
    public final IndirSetting<Boolean> SHOWENERGY;
    public final IndirSetting<Boolean> SHOWSTAM;
    public final IndirSetting<Boolean> SHOWCAL;
    public final IndirSetting<Boolean> SHOWHOTBAR1;
    public final IndirSetting<Boolean> SHOWHOTBAR2;
    public final IndirSetting<Boolean> SHOWHOTBAR3;
    public final IndirSetting<Boolean> SHOWMINIINV;
    public final IndirSetting<Boolean> SHOWMINIEQU;
    public final IndirSetting<Boolean> SHOWSTUDY;
    public final IndirSetting<Boolean> SHOWINVONLOGIN;
    public final IndirSetting<Boolean> SHOWBELTONLOGIN;
    public final IndirSetting<Boolean> SHOWSESSIONS;
    public final IndirSetting<Boolean> SHOWCHAT;
    public final IndirSetting<Boolean> SHOWMINIMAP;
    public final IndirSetting<Boolean> SHOWLRSLOTS;
    public final IndirSetting<Boolean> SHOWEXPWND;
    //Menugrid
    public final IndirSetting<Integer> MENUGRIDSIZEX;
    public final IndirSetting<Integer> MENUGRIDSIZEY;
    //Meter UI
    public final IndirSetting<Boolean> BIGSIMPLEMETERS;
    //Inv UI
    public final IndirSetting<Boolean> SHOWITEMQ;
    public final IndirSetting<Boolean> SHOWITEMWEAR;
    public final IndirSetting<Boolean> SHOWITEMCONT;
    public final IndirSetting<Boolean> ALWAYSITEMLONGTIPS;
    public final IndirSetting<Boolean> AUTOEQUIP;
    public final IndirSetting<Boolean> WATERDROPITEMCTRL;
    //Flowermenu UI
    public final IndirSetting<Boolean> QUICKFLMENU;
    public final IndirSetting<Boolean> KEEPFLOPEN;
    //Combat UI
    public final IndirSetting<Boolean> COLORIZEAGGRO;


    ////Temporary, non-saved, settings
    public final IndirSetting<Boolean> SHOWGRID;
    public final IndirSetting<Boolean> SHOWHIDDEN;
    public final IndirSetting<Boolean> SHOWHITBOX;
    public final IndirSetting<Boolean> SHOWHOVERTOOLTIPS;

    public SessionSettings(final String account, final String character) {
        final Settings local = new Settings(String.format("%s:%s", account, character));
        final Settings tmp = new Settings("Temp", false);
        //Display / Graphical
        COLORIZEAGGRO = new IndirSetting<>(local, "display.colorize-aggro", true);
        SHOWPCLAIM = new IndirSetting<>(local, "display.show-pclaim", false);
        SHOWVCLAIM = new IndirSetting<>(local, "display.show-vclaim", false);
        SHOWKCLAIM = new IndirSetting<>(local, "display.show-kclaim", false);

        //Minimap
        SHOWMMGOBS = new IndirSetting<>(local, "minimap.show-gobs", true);
        SHOWMMMARKERNAMES = new IndirSetting<>(local, "minimap.show-marker-names", true);
        SHOWMMGOBNAMES = new IndirSetting<>(local, "minimap.show-gob-names", true);
        SHOWMMMARKERS = new IndirSetting<>(local, "minimap.show-markers", true);
        SMALLMMMARKERS = new IndirSetting<>(local, "minimap.show-small-markers", true);
        SHOWPMARKERS = new IndirSetting<>(local, "minimap.show-placed-markers", true);
        SHOWNMARKERS = new IndirSetting<>(local, "minimap.show-natural-markers", true);
        SHOWCMARKERS = new IndirSetting<>(local, "minimap.show-custom-markers", true);
        SHOWLMARKERS = new IndirSetting<>(local, "minimap.show-linked-markers", true);
        SHOWKMARKERS = new IndirSetting<>(local, "minimap.show-kingdom-markers", true);
        SHOWKMARKERRAD = new IndirSetting<>(local, "minimap.show-kingdom-radius", true);
        SHOWVMARKERS = new IndirSetting<>(local, "minimap.show-village-markers", true);
        SHOWVMARKERRAD = new IndirSetting<>(local, "minimap.show-village-radius", true);
        SHOWVMARKERTIPS = new IndirSetting<>(local, "minimap.show-village-names", true);
        MMMEMSIZEONE = new IndirSetting<>(local, "minimap.mem-size-one", new Coord(100, 100));
        MMMEMPOSONE = new IndirSetting<>(local, "minimap.mem-pos-one", new Coord(500, 100));
        MMMEMSIZETWO = new IndirSetting<>(local, "minimap.mem-size-two", new Coord(300, 300));
        MMMEMPOSTWO = new IndirSetting<>(local, "minimap.mem-pos-two", new Coord(500, 100));
        MMSHOWGRID = new IndirSetting<>(local, "minimap.show-grid", false);
        MMSHOWVIEW = new IndirSetting<>(local, "minimap.show-view", true);
        MMPATHCOL = new IndirSetting<>(local, "minimap.path-color", Color.magenta);

        //Gob
        BADKIN = new IndirSetting<>(local, "gob.bad-kin-color", 2);
        COLORFULFARMES = new IndirSetting<>(local, "gob.colorful-frames", true);
        COLORFULTUBS = new IndirSetting<>(local, "gob.colorful-tubs", true);
        COLORFULCUPBOARDS = new IndirSetting<>(local, "gob.colorful-cupboards", true);
        COLORFULCHEESERACKS = new IndirSetting<>(local, "gob.colorful-cheese-racks", true);
        SHOWCROPSTAGE = new IndirSetting<>(local, "gob.show-crop-stage", false);
        SIMPLECROPS = new IndirSetting<>(local, "gob.simple-crops", false);
        GOBHIDDENCOL = new IndirSetting<>(local, "gob.hidden-col", new BaseColor(Color.WHITE));
        GOBHITBOXCOL = new IndirSetting<>(local, "gob.hitbox-col", new BaseColor(Color.WHITE));
        SHOWGOBHALO = new IndirSetting<>(local, "gob.show-gob-halo", false);
        SHOWGOBHALOONHEARTH = new IndirSetting<>(local, "gob.show-gob-halo-on-hearth", true);
        SHOWGOBHP = new IndirSetting<>(local, "gob.show-gob-hp", true);
        PATHWIDTH = new IndirSetting<>(local, "gob.path-width", 4);
        SHOWGOBPATH = new IndirSetting<>(local, "gob.show-gob-path", false);
        SHOWANIMALPATH = new IndirSetting<>(local, "gob.show-animal-path", false);
        SHOWANIMALRADIUS = new IndirSetting<>(local, "gob.show-animal-radius", false);
        GOBPATHCOL = new IndirSetting<>(local, "gob.gob-path-color", new BaseColor(Color.GREEN));
        ANIMALPATHCOL = new IndirSetting<>(local, "gob.animal-path-color", new BaseColor(Color.RED));
        VEHPATHCOL = new IndirSetting<>(local, "gob.vehicle-path-color", new BaseColor(Color.ORANGE));

        //Camera
        CAMERA = new IndirSetting<>(local, "camera.camera-type", "sortho");
        CAMERAPROJFAR = new IndirSetting<>(local, "camera.camera-proj-far", 5000);
        FREECAMREXAXIS = new IndirSetting<>(local, "camera.free.reverse-x-axis", false);
        FREECAMREYAXIS = new IndirSetting<>(local, "camera.free.reverse-y-axis", false);
        FREECAMLOCKELAV = new IndirSetting<>(local, "camera.free.lock-elevation", false);

        //Pathfinding
        PATHFINDINGTIER = new IndirSetting<>(local, "pathfinding.tier", 3);
        LIMITPATHFINDING = new IndirSetting<>(local, "pathfinding.limit-distance-to-view", false);
        RESEARCHUNTILGOAL = new IndirSetting<>(local, "pathfinding.research-until-at-goal", false);

        //UI
        SHOWPLAVA = new IndirSetting<>(local, "ui.show-player-avatar", true);
        SHOWSPEED = new IndirSetting<>(local, "ui.show-player-speed", true);
        SHOWHEALTH = new IndirSetting<>(local, "ui.show-player-health", true);
        SHOWENERGY = new IndirSetting<>(local, "ui.show-player-energy", true);
        SHOWSTAM = new IndirSetting<>(local, "ui.show-player-stam", true);
        SHOWCAL = new IndirSetting<>(local, "ui.show-calendar", true);
        SHOWHOTBAR1 = new IndirSetting<>(local, "ui.show-hotbar1", true);
        SHOWHOTBAR2 = new IndirSetting<>(local, "ui.show-hotbar2", true);
        SHOWHOTBAR3 = new IndirSetting<>(local, "ui.show-hotbar3", true);
        SHOWMINIINV = new IndirSetting<>(local, "ui.show-mini-inv", true);
        SHOWMINIEQU = new IndirSetting<>(local, "ui.show-mini-equ", true);
        SHOWSTUDY = new IndirSetting<>(local, "ui.show-study", true);
        SHOWSESSIONS = new IndirSetting<>(local, "ui.show-session-display", true);
        SHOWCHAT = new IndirSetting<>(local, "ui.show-chat", true);
        SHOWLRSLOTS = new IndirSetting<>(local, "ui.show-lr-hand-slots", true);
        SHOWMINIMAP = new IndirSetting<>(local, "ui.show-minimap", true);
        SHOWINVONLOGIN = new IndirSetting<>(local, "ui.show-inv-on-login", true);
        SHOWBELTONLOGIN = new IndirSetting<>(local, "ui.show-belt-on-login", true);
        MENUGRIDSIZEX = new IndirSetting<>(local, "ui.mg.size-x", 4);
        MENUGRIDSIZEY = new IndirSetting<>(local, "ui.mg.size-y", 4);
        BIGSIMPLEMETERS = new IndirSetting<>(local, "ui.big-simple-imeters", false);
        SHOWITEMQ = new IndirSetting<>(local, "ui.inv.show-item-quality", true);
        SHOWITEMWEAR = new IndirSetting<>(local, "ui.inv.show-item-wear", true);
        SHOWITEMCONT = new IndirSetting<>(local, "ui.inv.show-item-cont", true);
        ALWAYSITEMLONGTIPS = new IndirSetting<>(local, "ui.inv.always-show-longtip", true);
        AUTOEQUIP = new IndirSetting<>(local, "ui.inv.auto-equip", true);
        WATERDROPITEMCTRL = new IndirSetting<>(local, "ui.dont-drop-item-over-water", false);
        QUICKFLMENU = new IndirSetting<>(local, "ui.flowermenu.quick-menu", false);
        KEEPFLOPEN = new IndirSetting<>(local, "ui.flowermenu.never-close-on-click", false);
        SHOWEXPWND = new IndirSetting<>(local, "ui.show-experience-window", true);

        //Temporary, non-saved, settings
        SHOWHIDDEN = new IndirSetting<>(tmp, "session.show-hidden", false);
        SHOWGRID = new IndirSetting<>(tmp, "session.show-grid", false);
        SHOWHITBOX = new IndirSetting<>(tmp, "session.show-hitbox", false);
        SHOWHOVERTOOLTIPS = new IndirSetting<>(tmp, "session.show-hover-tooltips", false);
    }
}
