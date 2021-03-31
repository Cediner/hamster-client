package hamster.util.msg;

import java.util.*;

/**
 * Offices are purely to handle traffic between threads.
 */
public class Office {
    public final Thread owner;
    private final Queue<Transfer> transfers = new ArrayDeque<>();

    public Office(final Thread owner) {
	this.owner = owner;
    }

    public void processTransfers() {
	final List<Transfer> msgs = new ArrayList<>();
	synchronized (transfers) {
	    while (!transfers.isEmpty()) {
		msgs.add(transfers.remove());
	    }
	}

	for(final Transfer msg : msgs) {
	    msg.transfer();
	}
    }

    public void transfer(final Runnable job) {
	final Transfer trnmsg = new Transfer(job);
	synchronized (transfers) {
	    transfers.add(trnmsg);
	}
    }
}
