package hamster.util.msg;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.function.Consumer;

/**
 * Between threads should require a lock and be delayed by a frame
 * Within the same thread should be lock free and be retrievable instantly or the next frame
 *
 * A mailbox is owned by an Office (thread). The mailbox is only expected to be read/processed
 * from the thread that owns it. It can receive messages from other threads, but they may be delayed
 * by a frame if they aren't the thread that owns this mailbox.
 */
public class MailBox<T extends Message> {
    private final Office owner;
    private final Queue<T> box;

    public MailBox(final Office owner) {
	this.owner = owner;
	this.box = new ArrayDeque<>();
    }

    public boolean hasMail() {
        return !box.isEmpty();
    }

    public T receive() {
        return box.poll();
    }

    public void processMail(final Consumer<T> mailact) {
        while(hasMail()) {
            mailact.accept(receive());
	}
    }

    public void mail(final T msg) {
	if (owner.owner.equals(Thread.currentThread())) {
	    //box will only be written to within its own thread
	    box.add(msg);
	} else {
	    //Otherwise queued to the office which will handle a transfer at the start of the next frame
	    owner.transfer(() -> box.add(msg));
	}
    }
}
