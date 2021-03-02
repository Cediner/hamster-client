package hamster;

import haven.UI;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

public class KeyBind {
    private static final Settings datastore = new Settings("keybinds");
    public static final List<KeyBind> keybinds = new ArrayList<>();
    public static final KeyBind // UI
    	KB_TOGGLE_MINIMAP, KB_TOGGLE_INV, KB_TOGGLE_EQU, KB_TOGGLE_CHAR, KB_TOGGLE_KIN, KB_TOGGLE_OPTS,
    	KB_TOGGLE_CHAT, KB_TOGGLE_FORAGE, KB_TOGGLE_LIVESTOCK, KB_TOGGLE_CMD, KB_TOGGLE_PROFILER, KB_FOCUS_MAP,
    	KB_RECALL_MAP_ONE, KB_RECALL_MAP_TWO, KB_LOCK_ITEM_ON_MOUSE, KB_SCREENSHOT;
    public static final KeyBind // Gameplay
	KB_TOGGLE_GRID, KB_TOGGLE_TIPS, KB_TOGGLE_HITBOXES, KB_TOGGLE_HIDDEN, KB_TOGGLE_PAUSE,
	KB_QUICK_ACTION, KB_QUICK_BOARD, KB_EQ_HELD_INTO_LH, KB_EQ_HELD_INTO_RH;
    public static final KeyBind // Movement
    	KB_CYCLE_SPEED, KB_CRAWL, KB_WALK, KB_RUN, KB_SPRINT,
	KB_MOVE_NORTH, KB_MOVE_SOUTH, KB_MOVE_EAST, KB_MOVE_WEST;
    public static final KeyBind // Combat
    	KB_AGGRO_NEAREST_ANIMAL_TO_MOUSE, KB_AGGRO_NEAREST_PLAYER_TO_MOUSE,
	KB_TARGET_NEAREST_ANIMAL_TO_MOUSE, KB_TARGET_NEAREST_PLAYER_TO_MOUSE,
	KB_AGGRO_TARGET, KB_TARGET_CURRENT, KB_PEACE_CURRENT;
    public static final KeyBind[] KB_FIGHT_MOVE;
    public static final KeyBind // Camera
	    KB_RECENTER_CAMERA;
    //Hotkeys
    public static final KeyBind[] KB_HK_F, KB_HK_N, KB_HK;
    public static final KeyBind[] KB_HK_FP, KB_HK_NP, KB_HK_P;

    private static KeyBind addKB(final String name, final String grp, final String bind) {
        final KeyBind kb = new KeyBind(name, grp,
		new IndirSetting<>(datastore, name.replaceAll("\\s", "-").toLowerCase(), bind));
        keybinds.add(kb);
        return kb;
    }

    static {
	{ // UI Keybinds
	    final String UI_GRP = "UI";
	    KB_TOGGLE_MINIMAP = addKB("Toggle Minimap", UI_GRP, "C-A");
	    KB_TOGGLE_INV = addKB("Toggle Inventory", UI_GRP, "Tag");
	    KB_TOGGLE_EQU = addKB("Toggle Equipment", UI_GRP, "C-E");
	    KB_TOGGLE_CHAR = addKB("Toggle Charsheet", UI_GRP, "C-T");
	    KB_TOGGLE_KIN = addKB("Toggle Kin List", UI_GRP, "C-B");
	    KB_TOGGLE_OPTS = addKB("Toggle Options", UI_GRP, "C-O");
	    KB_TOGGLE_CHAT = addKB("Toggle Chat", UI_GRP, "C-C");
	    KB_TOGGLE_FORAGE = addKB("Toggle Forage", UI_GRP, "S-F");
	    KB_TOGGLE_LIVESTOCK = addKB("Toggle Livestock Manager", UI_GRP, "S-L");
	    KB_TOGGLE_CMD = addKB("Toggle Console", UI_GRP, "S-Semicolon");
	    KB_TOGGLE_PROFILER = addKB("Toggle Profiler", UI_GRP, "C-L");
	    KB_FOCUS_MAP = addKB("Focus Map", UI_GRP, "Escape");
	    KB_RECALL_MAP_ONE = addKB("Recall First map pos/size", UI_GRP, "C-Z");
	    KB_RECALL_MAP_TWO = addKB("Recall Second map pos/size", UI_GRP, "C-X");
	    KB_LOCK_ITEM_ON_MOUSE = addKB("Lock item on mouse", UI_GRP, "Back Quote");
	    KB_SCREENSHOT = addKB("Screenshot", UI_GRP, "M-S");
	}
	{ // Gameplay Keybinds
	    final String GP_GRP = "Gameplay";
	    KB_TOGGLE_GRID = addKB("Toggle Grid Lines", GP_GRP, "C-G");
	    KB_TOGGLE_TIPS = addKB("Toggle Hovertips", GP_GRP, "C-Q");
	    KB_TOGGLE_HITBOXES = addKB("Toggle Hitboxes", GP_GRP, "C-H");
	    KB_TOGGLE_HIDDEN = addKB("Toggle Gob When Hidden", GP_GRP, "S-G");
	    KB_TOGGLE_PAUSE = addKB("Toggle Pause", GP_GRP, "C-P");
	    KB_QUICK_ACTION = addKB("Quick Action", GP_GRP, "Q");
	    KB_QUICK_BOARD = addKB("Quick Board", GP_GRP, "S-Q");
	    KB_EQ_HELD_INTO_LH = addKB("Equip Held Item Into Left Hand", GP_GRP, "M-Z");
	    KB_EQ_HELD_INTO_RH = addKB("Eqip Held Item Into Right Hand", GP_GRP, "M-X");
	}
	{ // Movement Keybinds
	    final String MV_GRP = "Movement";
	    KB_CYCLE_SPEED = addKB("Cycle Character Speed", MV_GRP, "C-R");
	    KB_CRAWL = addKB("Change to Crawl Speed", MV_GRP, "M-Q");
	    KB_WALK = addKB("Change to Walk Speed", MV_GRP, "M-W");
	    KB_RUN = addKB("Change to Run Speed", MV_GRP, "M-E");
	    KB_SPRINT = addKB("Change to Sprint Speed", MV_GRP, "M-R");
	    KB_MOVE_NORTH = addKB("Move North", MV_GRP, "Up");
	    KB_MOVE_SOUTH = addKB("Move South", MV_GRP, "Down");
	    KB_MOVE_EAST = addKB("Move East", MV_GRP, "Left");
	    KB_MOVE_WEST = addKB("Move West", MV_GRP, "Right");
	}
	{ // Combat Keybinds
	    final String CBT_GRP = "Combat";
	    KB_AGGRO_NEAREST_ANIMAL_TO_MOUSE = addKB("Aggro animal nearest to mouse", CBT_GRP, "C-F");
	    KB_AGGRO_NEAREST_PLAYER_TO_MOUSE = addKB("Aggro player nearest to mouse", CBT_GRP, "C-D");
	    KB_TARGET_NEAREST_ANIMAL_TO_MOUSE = addKB("Target nearest animal to mouse", CBT_GRP, "S-A");
	    KB_TARGET_NEAREST_PLAYER_TO_MOUSE = addKB("Target nearest player to mouse", CBT_GRP, "S-D");
	    KB_AGGRO_TARGET = addKB("Aggro targeted gob", CBT_GRP, "S-T");
	    KB_TARGET_CURRENT = addKB("Target current gob", CBT_GRP, "S-C");
	    KB_PEACE_CURRENT = addKB("Peace current target", CBT_GRP, "S-P");
	    final int moves = 10;
	    KB_FIGHT_MOVE = new KeyBind[moves];
	    for(var i = 1; i < moves; ++i) {
		KB_FIGHT_MOVE[i-1] = addKB("Fight Move " + i, CBT_GRP, Integer.toString(i));
	    }
	}
	{ // Camera Keybinds
	    final String CAM_GRP = "Camera";
	    KB_RECENTER_CAMERA = addKB("Recenter Camera on Player", CAM_GRP, "S-C");
	}
	{ // Setup Hotkey keybinds
	    final String HB_GRP = "Hotbar";
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
	            hotkeys[j-1][i-1] = addKB("Hotbar " + j + " - Slot " + i, HB_GRP, prefix[j-1]+i);
		}
	        for(var i = 1; i < pages[j-1].length; i++) {
	            pages[j-1][i-1] = addKB("Hotbar " + j + " - Page " + i, HB_GRP, "");
		}
	    }
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
	return ibind.equals(bind.get()) && action.run();
    }

    public static boolean validBinding(final String binding) {
	if (binding.equals("")) {
	    return true;
	} else {
	    for (final var kb : keybinds) {
		if (!kb.bind.get().equals(binding)) {
		    return true;
		}
	    }
	}
	return false;
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
