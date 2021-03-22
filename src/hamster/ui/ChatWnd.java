package hamster.ui;

import hamster.ui.core.ResizableWnd;
import haven.ChatUI;
import haven.Coord;

public class ChatWnd extends ResizableWnd {
    private static final Coord minsz = new Coord(300, 150);
    private final ChatUI chat;

    public ChatWnd(final ChatUI chat) {
        super(chat.sz, "Chat");
        this.chat = chat;
        add(chat, Coord.z);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void toggleVisibility() {
        super.toggleVisibility();
        ui.gui.settings.SHOWCHAT.set(visible);
    }

    @Override
    protected void added() {
        super.added();
        chat.resize(asz);
        setVisible(ui.gui.settings.SHOWCHAT.get());
    }

    @Override
    public void resize(Coord sz) {
        sz = sz.max(minsz);
        super.resize(sz);
        chat.resize(asz);
    }
}
