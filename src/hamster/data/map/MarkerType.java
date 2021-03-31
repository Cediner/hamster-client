package hamster.data.map;

public enum MarkerType {
    PLAYER_MARKER(0),
    SERVER_MARKER(1),
    CUSTOM_MARKER(2),
    REALM_MARKER(3),
    LINKED_MARKER(4),
    SCRIPT_MARKER(5);

    public final byte id;

    MarkerType(final int id) {
        this.id = (byte) id;
    }
}
