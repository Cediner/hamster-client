package hamster;

import hamster.data.TranslationLookup;
import haven.UI;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class KeyBind {
    private static final Settings datastore = new Settings("keybinds", new HashMap<>());
    private static final List<KeyBind> keybinds = new ArrayList<>();
    private static final Map<String, KeyBind> dynamicKeyBinds = new HashMap<>();
    public static final KeyBind // UI
    	KB_TOGGLE_MINIMAP, KB_TOGGLE_INV, KB_TOGGLE_EQU, KB_TOGGLE_CHAR, KB_TOGGLE_KIN, KB_TOGGLE_OPTS,
    	KB_TOGGLE_CHAT, KB_TOGGLE_FORAGE, KB_TOGGLE_LIVESTOCK, KB_TOGGLE_CMD, KB_TOGGLE_PROFILER, KB_FOCUS_MAP,
    	KB_RECALL_MAP_ONE, KB_RECALL_MAP_TWO, KB_LOCK_ITEM_ON_MOUSE, KB_SCREENSHOT, KB_SESSION_CYCLE_BACK,
	KB_SESSION_CYCLE_FORWARD,
    	//Misc UI stuff
    	KB_MAKE_ONE, KB_MAKE_ALL, KB_SCM_ROOT, KB_SCM_BACK, KB_SCM_NEXT;
    public static final KeyBind // Gameplay
	KB_TOGGLE_GRID, KB_TOGGLE_TIPS, KB_TOGGLE_HITBOXES, KB_TOGGLE_HIDDEN, KB_TOGGLE_PAUSE,
	KB_QUICK_ACTION, KB_QUICK_BOARD, KB_EQ_HELD_INTO_LH, KB_EQ_HELD_INTO_RH;
    public static final KeyBind // Movement
    	KB_CYCLE_SPEED, KB_CRAWL, KB_WALK, KB_RUN, KB_SPRINT,
	KB_MOVE_NORTH, KB_MOVE_SOUTH, KB_MOVE_EAST, KB_MOVE_WEST;
    public static final KeyBind // Combat
    	KB_AGGRO_NEAREST_ANIMAL_TO_MOUSE, KB_AGGRO_NEAREST_PLAYER_TO_MOUSE,
	KB_TARGET_NEAREST_ANIMAL_TO_MOUSE, KB_TARGET_NEAREST_PLAYER_TO_MOUSE,
	KB_AGGRO_TARGET, KB_TARGET_CURRENT, KB_PEACE_CURRENT, KB_CYCLEUP_OPP, KB_CYCLEDOWN_OPP;
    public static final KeyBind[] KB_FIGHT_MOVE;
    public static final KeyBind // Camera
     	KB_RECENTER_CAMERA, KB_CAM_RESET, KB_CAM_LEFT, KB_CAM_RIGHT, KB_CAM_IN, KB_CAM_OUT;
    public static final KeyBind // MapWnd
    	KB_MAP_HOME, KB_MAP_MARK, KB_MAP_HIDE_MARKERS, KB_MAP_COMPACT;

    //Hotkeys
    public static final KeyBind[] KB_HK_F, KB_HK_N, KB_HK;
    public static final KeyBind[] KB_HK_FP, KB_HK_NP, KB_HK_P;

    private static KeyBind addKB(final String display, final String name, final String grp, final String bind) {
	final KeyBind kb = new KeyBind(display, grp,
		new IndirSetting<>(datastore, name.replaceAll("\\s", "-").toLowerCase(), bind));
	keybinds.add(kb);
	return kb;
    }

    private static KeyBind addKB(final String name, final String grp, final String bind) {
        return addKB(TranslationLookup.get(name), name, grp, bind);
    }

    public static KeyBind getDynamicKB(final String name, final String grp, final String bind) {
        final String key = grp + "/" + name;
	if(dynamicKeyBinds.containsKey(key)) {
	    return dynamicKeyBinds.get(key);
	} else {
	    final var set = new IndirSetting<>(datastore, key, bind);
	    final KeyBind kb = new KeyBind(name, grp, set);
	    dynamicKeyBinds.put(key, kb);
	    return kb;
	}
    }

    public static KeyBind getFinalKB(final String name, final String bind) {
        return new KeyBind(name, "",
		new IndirSetting<>(datastore, "final/"+name.replaceAll("\\s", "-").toLowerCase(), bind));
    }

    static {
	{ // UI Keybinds
	    final String UI_GRP = TranslationLookup.get("kb_ui");
	    KB_TOGGLE_MINIMAP = addKB("kb_ui_mm", UI_GRP, "C-A"); //GameUI
	    KB_TOGGLE_INV = addKB("kb_ui_inv", UI_GRP, "Tab"); //GameUI
	    KB_TOGGLE_EQU = addKB("kb_ui_equ", UI_GRP, "C-E"); //GameUI
	    KB_TOGGLE_CHAR = addKB("kb_ui_char", UI_GRP, "C-T"); //GameUI
	    KB_TOGGLE_KIN = addKB("kb_ui_kin", UI_GRP, "C-B"); //GameUI
	    KB_TOGGLE_OPTS = addKB("kb_ui_opts", UI_GRP, "C-O"); //GameUI
	    KB_TOGGLE_CHAT = addKB("kb_ui_chat", UI_GRP, "C-C"); //GameUI
	    KB_TOGGLE_FORAGE = addKB("kb_ui_forage", UI_GRP, "S-F");
	    KB_TOGGLE_LIVESTOCK = addKB("kb_ui_livestock", UI_GRP, "S-L");
	    KB_TOGGLE_CMD = addKB("kb_ui_cmd", UI_GRP, "S-Semicolon"); //GameUI / Root
	    KB_TOGGLE_PROFILER = addKB("kb_ui_profiler", UI_GRP, "C-L");
	    KB_FOCUS_MAP = addKB("kb_ui_focus_map", UI_GRP, "Escape"); //GameUI
	    KB_RECALL_MAP_ONE = addKB("kb_ui_recall_map_one", UI_GRP, "C-Z"); //MapWnd
	    KB_RECALL_MAP_TWO = addKB("kb_ui_recall_map_two", UI_GRP, "C-X"); //MapWnd
	    KB_LOCK_ITEM_ON_MOUSE = addKB("kb_ui_lock_item", UI_GRP, "Back Quote");
	    KB_SCREENSHOT = addKB("kb_ui_screenshot", UI_GRP, "M-S"); //GameUI
	    KB_MAKE_ONE = addKB("kb_ui_make_one", UI_GRP, "Enter");
	    KB_MAKE_ALL = addKB("kb_ui_make_all", UI_GRP, "C-Enter");
	    KB_SCM_ROOT = addKB("kb_ui_scm_root", UI_GRP, "S-Escape");
	    KB_SCM_BACK = addKB("kb_ui_scm_back", UI_GRP, "Backspace");
	    KB_SCM_NEXT = addKB("kb_ui_scm_next", UI_GRP, "N");
	    KB_SESSION_CYCLE_BACK = addKB("kb_ui_session_cycle_back", UI_GRP, "C-Left");
	    KB_SESSION_CYCLE_FORWARD = addKB("kb_ui_session_cycle_forward", UI_GRP, "C-Right");
	}
	{ // Gameplay Keybinds
	    final String GP_GRP = TranslationLookup.get("kb_gameplay");
	    KB_TOGGLE_GRID = addKB("kb_gameplay_grid", GP_GRP, "C-G");
	    KB_TOGGLE_TIPS = addKB("kb_gameplay_tips", GP_GRP, "C-Q");
	    KB_TOGGLE_HITBOXES = addKB("kb_gameplay_hitboxes", GP_GRP, "C-H");
	    KB_TOGGLE_HIDDEN = addKB("kb_gameplay_hidden", GP_GRP, "S-G");
	    KB_TOGGLE_PAUSE = addKB("kb_gameplay_pause", GP_GRP, "C-P"); //Root
	    KB_QUICK_ACTION = addKB("kb_gameplay_quick_action", GP_GRP, "Q");
	    KB_QUICK_BOARD = addKB("kb_gameplay_quick_board", GP_GRP, "S-Q");
	    KB_EQ_HELD_INTO_LH = addKB("kb_gameplay_eq_held_into_lh", GP_GRP, "M-Z");
	    KB_EQ_HELD_INTO_RH = addKB("kb_gameplay_eq_held_into_rh", GP_GRP, "M-X");
	}
	{ // Movement Keybinds
	    final String MV_GRP = TranslationLookup.get("kb_move");
	    KB_CYCLE_SPEED = addKB("kb_move_cycle_speed", MV_GRP, "C-R"); //Speedget
	    KB_CRAWL = addKB("kb_move_crawl", MV_GRP, "M-Q"); //Speedget
	    KB_WALK = addKB("kb_move_walk", MV_GRP, "M-W"); //Speedget
	    KB_RUN = addKB("kb_move_run", MV_GRP, "M-E"); //Speedget
	    KB_SPRINT = addKB("kb_move_sprint", MV_GRP, "M-R"); //Speedget
	    KB_MOVE_NORTH = addKB("kb_move_north", MV_GRP, "S-Up");
	    KB_MOVE_SOUTH = addKB("kb_move_south", MV_GRP, "S-Down");
	    KB_MOVE_EAST = addKB("kb_move_east", MV_GRP, "S-Left");
	    KB_MOVE_WEST = addKB("kb_move_west", MV_GRP, "S-Right");
	}
	{ // Combat Keybinds
	    final String CBT_GRP = TranslationLookup.get("kb_combat");
	    KB_AGGRO_NEAREST_ANIMAL_TO_MOUSE = addKB("kb_combat_aggro_nearest_animal", CBT_GRP, "C-F");
	    KB_AGGRO_NEAREST_PLAYER_TO_MOUSE = addKB("kb_combat_aggro_nearest_player", CBT_GRP, "C-D");
	    KB_TARGET_NEAREST_ANIMAL_TO_MOUSE = addKB("kb_combat_target_nearest_animal", CBT_GRP, "S-A");
	    KB_TARGET_NEAREST_PLAYER_TO_MOUSE = addKB("kb_combat_target_nearest_player", CBT_GRP, "S-D");
	    KB_AGGRO_TARGET = addKB("kb_combat_aggro_target", CBT_GRP, "S-T");
	    KB_TARGET_CURRENT = addKB("kb_combat_target_current", CBT_GRP, "S-C");
	    KB_PEACE_CURRENT = addKB("kb_combat_peace_current", CBT_GRP, "S-P");
	    KB_CYCLEUP_OPP = addKB("kb_combat_cycleup_opp", CBT_GRP, "");
	    KB_CYCLEDOWN_OPP = addKB("kb_combat_cycledown_opp", CBT_GRP, "");
	    final int moves = 10;
	    KB_FIGHT_MOVE = new KeyBind[moves];
	    for(var i = 1; i <= moves; ++i) {
		KB_FIGHT_MOVE[i-1] = addKB(String.format("%s %d", TranslationLookup.get("kb_combat_fight_move"), i), "Fight Move " + i, CBT_GRP, i == 10 ? "0" : Integer.toString(i));
	    }
	}
	{ // Camera Keybinds
	    final String CAM_GRP = TranslationLookup.get("kb_camera");
	    KB_RECENTER_CAMERA = addKB("kb_camera_recenter", CAM_GRP, "S-C");
	    KB_CAM_RESET = addKB("kb_camera_reset", CAM_GRP, "Home");
	    KB_CAM_LEFT = addKB("kb_camera_left", CAM_GRP, "Left");
	    KB_CAM_RIGHT = addKB("kb_camera_right", CAM_GRP, "Right");
	    KB_CAM_IN = addKB("kb_camera_in", CAM_GRP, "Up");
	    KB_CAM_OUT = addKB("kb_camera_out", CAM_GRP, "Down");
	}
	{ // Setup Hotkey keybinds
	    final String HB_GRP = TranslationLookup.get("kb_hotbar");
	    final int slots = 10;
	    KB_HK_F = new KeyBind[slots];
	    KB_HK_FP = new KeyBind[5];
	    KB_HK_N = new KeyBind[slots];
	    KB_HK_NP = new KeyBind[4];
	    KB_HK = new KeyBind[slots];
	    KB_HK_P = new KeyBind[5];
	    final KeyBind[][] hotkeys = { KB_HK_F, KB_HK_N, KB_HK };
	    final KeyBind[][] pages = { KB_HK_FP, KB_HK_NP, KB_HK_P };
	    final String[] prefix = { "F", "NumPad-", "" };
	    for(var j = 1; j <= hotkeys.length; j++) {
	        for(var i = 1; i <= hotkeys[j-1].length; i++) {
	            hotkeys[j-1][i-1] = addKB(String.format("%s %d - %s %d", TranslationLookup.get("kb_hotbar"), j, TranslationLookup.get("kb_hotbar_slot"), i),
			    String.format("Hotbar %d - Slot %d", j, i),
			    HB_GRP,
			    prefix[j-1]+(i == 10 ? "0" : Integer.toString(i)));
		}
	        for(var i = 1; i < pages[j-1].length; i++) {
	            pages[j-1][i-1] = addKB(String.format("%s %d - %s %d", TranslationLookup.get("kb_hotbar"), j, TranslationLookup.get("kb_hotbar_page"), i),
			    String.format("Hotbar %d - Page %d", j, i), HB_GRP, "");
		}
	    }
	}
	{ // MapWnd Keybinds
	    final String MAP_GRP = TranslationLookup.get("kb_mm");
	    KB_MAP_HOME = addKB("kb_mm_home", MAP_GRP, "C-Home");
	    KB_MAP_MARK = addKB("kb_mm_mark", MAP_GRP, "");
	    KB_MAP_HIDE_MARKERS = addKB("kb_mm_hide_markers", MAP_GRP, "C-M");
	    KB_MAP_COMPACT = addKB("kb_mm_compact", MAP_GRP, "");
	}
    }

    @FunctionalInterface
    public interface Command {
	boolean run();
    }

    // Visual Name
    public final String name;
    // Grouping identifier for UI
    public final String grouping;
    // Actual bind sequence
    public final IndirSetting<String> bind;

    public KeyBind(final String name, final String grouping, final IndirSetting<String> bind) {
	this.name = name;
	this.grouping = grouping;
	this.bind = bind;
    }

    public boolean check(final String ibind, final Command action) {
	return !ibind.equals("") && ibind.equals(bind.get()) && action.run();
    }

    public boolean match(final String ibind) {
        return !ibind.equals("") && ibind.equals(bind.get());
    }

    public static boolean validBinding(final IndirSetting<String> ignore, final String binding) {
	if (binding.equals("")) {
	    return true;
	} else {
	    for (final var kb : keybinds) {
		if (kb.bind != ignore && kb.bind.get().equals(binding)) {
		    return false;
		}
	    }
	}
	return true;
    }

    public static Map<String, List<KeyBind>> generateGroupings() {
        final Map<String, List<KeyBind>> groups = new HashMap<>();
        for(final var kb : keybinds) {
            if(groups.containsKey(kb.grouping)) {
                groups.get(kb.grouping).add(kb);
	    } else {
                final List<KeyBind> kbs = new ArrayList<>();
                kbs.add(kb);
                groups.put(kb.grouping, kbs);
	    }
	}
        return groups;
    }

    public static String generateSequence(final KeyEvent ev, final UI ui) {
	switch (ev.getKeyCode()) {
	    case 0x0:
	    case KeyEvent.VK_SHIFT:
	    case KeyEvent.VK_CONTROL:
	    case KeyEvent.VK_META:
	    case KeyEvent.VK_ALT:
		return "";
	    default: {
		final StringBuilder keyseq = new StringBuilder();
		if (ui.modshift)
		    keyseq.append("S-");
		if (ui.modctrl)
		    keyseq.append("C-");
		if (ui.modmeta)
		    keyseq.append("M-");
		keyseq.append(KeyEvent.getKeyText(ev.getKeyCode()));
		return keyseq.toString();
	    }
	}
    }
}
