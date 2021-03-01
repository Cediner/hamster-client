package hamster.util;

import java.util.*;

/**
 * Between threads should require a lock and be delayed by a frame
 * Within the same thread should be lock free and be retrievable instantly or the next frame
 */
public class MessageBus {
    public static abstract class Message { }

    private static class Transfer extends Message {
        public final Runnable job;
        public Transfer(final Runnable job) {
            this.job = job;
        }

        public void transfer() {
            job.run();
        }
    }

    public static class MailBox<T extends Message> {
        public final Office owner;
        public final Queue<T> mailqueue;

        public MailBox(final Office owner, final Object id) {
            this.owner = owner;
            this.mailqueue = new ArrayDeque<>();
            owner.register(this, id);
        }

        public void mail(final T msg) {
            if(owner.owner.equals(Thread.currentThread())) {
                mailqueue.add(msg);
            } else {
                owner.transfer(() -> mailqueue.add(msg));
            }
        }
    }

    public static class Office {
        public final Thread owner;
        private final Map<Object, MailBox<? extends Message>> boxes = new HashMap<>();
        private final Queue<Transfer> transfers = new ArrayDeque<>();

        public Office(final Thread owner) {
            this.owner = owner;
        }

        public <T extends Message> void register(final MailBox<T> mb, final Object id) {
            boxes.put(id, mb);
        }

        public void unregister(final Object id) {
            boxes.remove(id);
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
}
