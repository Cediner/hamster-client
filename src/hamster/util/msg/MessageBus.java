package hamster.util.msg;

import java.util.ArrayList;
import java.util.Collection;

/**
 * MessageBus are like MailBoxes but instead of processes the Messages
 * themselves they disperse it to other mailboxes that are subscribed
 * to itself. This is a useful method of dispersing Messages when
 * you don't have access directly to the MailBox(es) that need to
 * receive it (ie: cross sessions)
 * @param <T> The type of messages that will pass through here
 *           MailBoxes that subscribe are expected to be of the
 *           same type as well
 */
public class MessageBus<T extends Message> {
    private final Collection<MailBox<T>> members = new ArrayList<>();

    public void subscribe(final MailBox<T> mailbox) {
	synchronized (members) {
	    members.add(mailbox);
	}
    }

    public void unsubscribe(final MailBox<T> mailbox) {
	synchronized (members) {
	    members.remove(mailbox);
	}
    }

    public void send(final T msg) {
	synchronized (members) {
	    for(final var member : members) {
		member.mail(msg);
	    }
	}
    }
}
